package co.com.sura.postgres.repository.agenda.adapter;

import co.com.sura.autoagendador.AutoAgendador;
import co.com.sura.autoagendador.OrigenRegional;
import co.com.sura.autoagendador.Resultado;
import co.com.sura.entity.agenda.AgendamientoAutomaticoRepository;
import co.com.sura.entity.agenda.TiposDesplazamientos;
import co.com.sura.entity.moviles.Desplazamiento;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.mapbox.repository.map.config.ConfigMapBox;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.agenda.data.CitaRepository;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoRepository;
import co.com.sura.postgres.repository.personal.data.ProfesionalData;
import co.com.sura.postgres.repository.personal.data.ProfesionalRepository;
import co.com.sura.services.mapbox.GeoUbicacion;
import co.com.sura.services.mapbox.MapboxServiceRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;

import static co.com.sura.autoagendador.IdRegional.getByIdCiudad;
import static co.com.sura.genericos.EstadosCita.AGENDADA;
import static co.com.sura.postgres.repository.moviles.data.DesplazamientoData.crearDesplazamientoData;

@Repository
public class AgendamientoAutomaticoAdapter implements AgendamientoAutomaticoRepository {

    private final CitaRepository citaRepository;
    private final ProfesionalRepository profesionalRepository;
    private final DesplazamientoRepository desplazamientoRepository;
    private final MapboxServiceRepository mapboxService;

    @Autowired
    public AgendamientoAutomaticoAdapter(CitaRepository citaRepository, ProfesionalRepository profesionalRepository,
                                         DesplazamientoRepository desplazamientoRepository,
                                         MapboxServiceRepository mapboxService) {
        this.citaRepository = citaRepository;
        this.profesionalRepository = profesionalRepository;
        this.desplazamientoRepository = desplazamientoRepository;
        this.mapboxService = mapboxService;
    }

    @Override
    public Mono<Boolean> autoagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad) {
        return desagendarTurnoCompleto(fechaTurno,idHorarioTurno,idCiudad)
           .then(citaRepository
              .findCitasByTurnoRegionalHorario(fechaTurno,idHorarioTurno,idCiudad, EstadosCita.CANCELADA.getEstado())
              .collectList()
              .flatMap(citaDataList -> profesionalRepository.findFromTurnoRegional(fechaTurno,idCiudad,idHorarioTurno)
                .collectList().flatMap(profesionalesDataList -> {
                   var autoAgendador = new AutoAgendador(
                   OrigenRegional.getOrigenCiudadById(getByIdCiudad(idCiudad)).getCitaGenetic(),
                   ConverterAgenda.convertToListCitaGenetic(citaDataList), profesionalesDataList.size(),
                   ConfigMapBox.NUMERO_GENERACIONES, ConfigMapBox.SIZE_POBLACION_INICIAL,
                   ConfigMapBox.NUMERO_PADRES_EMPAREJADOS, ConfigMapBox.PENALIZACION_HOLGURA_NEGATIVA, mapboxService);

                   autoAgendador.run();
                   return asignarListaCitaToProfesionalAutoagendar(profesionalesDataList,autoAgendador.mejorSolucion());
                })))
           .then(insertDesplazamientosAllCitasByProfesional(fechaTurno,idCiudad,idHorarioTurno))
           .then(Mono.just(Boolean.TRUE));
    }

    @Override
    public Flux<Desplazamiento> consultarDesplazamientoByCitaPartida(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idCiudad){
        return desplazamientoRepository.findAllByturno(fechaProgramada,idHorarioTurno,idCiudad)
                .map(ConverterAgenda :: converToDesplazamiento);
    }

    @Override
    public Mono<Boolean> desagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idRegional) {
        return desplazamientoRepository.deleteAllByFechaTurno(fechaTurno,idHorarioTurno,idRegional)
               .then(citaRepository
                    .desagendarTurnoCompleto(fechaTurno,idHorarioTurno,idRegional,EstadosCita.SIN_AGENDAR.getEstado()))
               .then(Mono.just(Boolean.TRUE));
    }

    private Mono<Void> asignarListaCitaToProfesionalAutoagendar (
            List<ProfesionalData> profesionalesDataList, Resultado mejoResultado){
        return Flux.zip(
                    Flux.fromIterable(profesionalesDataList),
                    Flux.fromIterable(mejoResultado.getIndividuo().getCitaGen()))
               .flatMap(tuple-> Flux.fromIterable(tuple.getT2())
                                   .flatMap(cita-> citaRepository
                                           .updateEstadoAndProfesional(cita.getIdCita(),
                                                   AGENDADA.getEstado(),
                                                   tuple.getT1().getNumeroIdentificacion())))
                .then();
    }
    private Mono<Void> insertDesplazamientosAllCitasByProfesional(
            LocalDate fechaTurno, String idCiudad,Integer idHorarioTurno){

        return profesionalRepository.findFromTurnoRegional(fechaTurno,idCiudad,idHorarioTurno)
                .collectList()
                .flatMap(profesionalListData -> Flux.fromIterable(profesionalListData)
                        .flatMap(profesionalData -> calcularDesplazamientoCitaByProfesional(
                                fechaTurno, idHorarioTurno, idCiudad, profesionalData.getNumeroIdentificacion()))
                        .then());
    }
    @Override
    public Mono<Boolean> calcularDesplazamientoCitaByProfesional(
            LocalDate fechaTurno, Integer idHorarioTurno,String idRegional, String idProfesional) {

        return desplazamientoRepository.deleteByFechaTurnoProfesional(fechaTurno,idHorarioTurno,idProfesional)
                .then(citaRepository.findCitasByTurnoRegionalProfesional(
                                fechaTurno,idHorarioTurno,idRegional,idProfesional,EstadosCita.CANCELADA.getEstado())
                        .collectList()
                        .flatMapMany(Flux::fromIterable)
                        .buffer(ConfigMapBox.MAXSIZE,1)
                        .filter(citas -> citas.size() == ConfigMapBox.MAXSIZE)
                        .map(citas -> {
                            CitaData citaPartida = citas.get(0);
                            CitaData citaDestino = citas.get(1);

                            var duracionViaje = mapboxService.calcularTiempoViaje(
                                    new GeoUbicacion(citaPartida.getLatitud(),citaPartida.getLongitud()),
                                    new GeoUbicacion(citaDestino.getLatitud(),citaDestino.getLongitud())).block();

                            return  crearDesplazamientoData(citaPartida,citaDestino).toBuilder()
                                    .tipo(TiposDesplazamientos.dvisita.name())
                                    .idHorarioTurno(idHorarioTurno)
                                    .duracion(duracionViaje)
                                    .holgura(ConfigMapBox.HOLGURA_DEFECTO)
                                    .build();
                        })
                        .flatMap(desplazamientoRepository::save)
                        .then(Mono.just(Boolean.TRUE)));
    }
}
