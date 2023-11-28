package co.com.sura.postgres.repository.agenda.adapter;

import co.com.sura.autoagendador.models.AutoAgendador;
import co.com.sura.autoagendador.models.CitaGenetic;
import co.com.sura.autoagendador.models.Resultado;
import co.com.sura.entity.agenda.AgendamientoAutomaticoRepository;
import co.com.sura.entity.agenda.TiposDesplazamientos;
import co.com.sura.entity.maestro.HorarioTurno;
import co.com.sura.entity.moviles.Desplazamiento;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.autoagendador.config.Config;
import co.com.sura.genericos.Numeros;
import co.com.sura.postgres.repository.agenda.data.CitaRepository;
import co.com.sura.postgres.repository.maestros.adapter.MaestroRepositoryAdapter;
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
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;

import static co.com.sura.genericos.EstadosCita.AGENDADA;
import static co.com.sura.postgres.repository.moviles.data.DesplazamientoData.crearDesplazamientoData;

@Repository
public class AgendamientoAutomaticoAdapter implements AgendamientoAutomaticoRepository {

    private final CitaRepository citaRepository;
    private final ProfesionalRepository profesionalRepository;
    private final DesplazamientoRepository desplazamientoRepository;
    private final MaestroRepositoryAdapter maestroRepositoryAdapter;
    private final MapboxServiceRepository mapboxService;
    private final AutoAgendador autoAgendador;
    @Autowired
    public AgendamientoAutomaticoAdapter(CitaRepository citaRepository, ProfesionalRepository profesionalRepository,
                                         DesplazamientoRepository desplazamientoRepository,
                                         MaestroRepositoryAdapter maestroRepositoryAdapter,
                                         MapboxServiceRepository mapboxService, AutoAgendador autoAgendador) {
        this.citaRepository = citaRepository;
        this.profesionalRepository = profesionalRepository;
        this.desplazamientoRepository = desplazamientoRepository;
        this.maestroRepositoryAdapter = maestroRepositoryAdapter;
        this.mapboxService = mapboxService;
        this.autoAgendador = autoAgendador;
    }
    @Override
    public Mono<Boolean> autoagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idRegional) {
        return desagendarTurnoCompleto(fechaTurno, idHorarioTurno, idRegional)
                .then(
                   Mono.zip(
                      citaRepository.findCitasByTurnoRegionalHorario(
                              fechaTurno,idHorarioTurno,idRegional, EstadosCita.CANCELADA.getEstado()).collectList(),

                      profesionalRepository.findFromTurnoRegional(fechaTurno,idRegional,idHorarioTurno).collectList(),

                      buildCitaOrigen(fechaTurno,idRegional,idHorarioTurno))

                   .flatMap(tuple->{
                       autoAgendador.withCitas(ConverterAgenda.convertToListCitaGenetic(tuple.getT1()))
                               .andOrigen(tuple.getT3())
                               .andNumeroMoviles(tuple.getT2().size())
                               .run();
                       var mejorSolucion = autoAgendador.mejorSolucion();
                       autoAgendador.resetData();
                       return asignarListaCitaToProfesionalAutoagendar(tuple.getT2(),mejorSolucion);
                   }))
                .then(insertDesplazamientosAllCitasByProfesional(fechaTurno, idRegional, idHorarioTurno))
                .onErrorResume(e-> desagendarTurnoCompleto(fechaTurno, idHorarioTurno, idRegional)
                        .then(desplazamientoRepository.deleteAllByFechaTurno(fechaTurno,idHorarioTurno,idRegional))
                        .then(Mono.error(e)))
                .then(Mono.just(Boolean.TRUE));
    }

    private Mono<CitaGenetic> buildCitaOrigen(LocalDate fechaTurno,String idRegional, Integer idHorarioTurno){
        return Mono.zip(
           maestroRepositoryAdapter.consultarRegionalById(idRegional),
           maestroRepositoryAdapter.consultarHorarioTurnoById(idHorarioTurno)
                        .filter(HorarioTurno::getEsHorarioBase))
           .map(tuple-> CitaGenetic.builder()
                   .idCita(tuple.getT1().getId())
                   .latitud(tuple.getT1().getLatitud())
                   .longitud(tuple.getT1().getLongitud())
                   .duracion(0)
                   .holgura(Numeros.NOVECIENTOS_SEGUNDOS.getValue())
                  .fechaInicioIso(LocalDateTime.of(fechaTurno,tuple.getT2().getHoraInicio())
                           .toEpochSecond(ZoneOffset.UTC))
                   .build());

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
            LocalDate fechaTurno, String idRegional,Integer idHorarioTurno){

        return profesionalRepository.findFromTurnoRegional(fechaTurno,idRegional,idHorarioTurno)
                .collectList()
                .flatMap(profesionalListData ->
                         Flux.fromIterable(profesionalListData)
                        .flatMap(profesionalData -> calcularDesplazamientoCitaByProfesional(
                                fechaTurno, idHorarioTurno, idRegional, profesionalData.getNumeroIdentificacion()))
                        .then());
    }
    @Override
    public Flux<Desplazamiento> consultarDesplazamientoByCitaPartida(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idRegional){
        return desplazamientoRepository.findByFechaProgramada(fechaProgramada,idRegional,idHorarioTurno)
                .map(ConverterAgenda :: converToDesplazamiento);
    }

    @Override
    public Mono<Boolean> desagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idRegional) {
        return desplazamientoRepository.deleteAllByFechaTurno(fechaTurno,idHorarioTurno,idRegional)
               .then(citaRepository
                    .desagendarTurnoCompleto(fechaTurno,idHorarioTurno,idRegional,EstadosCita.SIN_AGENDAR.getEstado()))
               .then(Mono.just(Boolean.TRUE));
    }

    @Override
    public Mono<Boolean> calcularDesplazamientoCitaByProfesional(
            LocalDate fechaTurno, Integer idHorarioTurno,String idRegional, String idProfesional) {

       return desplazamientoRepository.deleteByFechaTurnoProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional)
               .then(Mono.zip(
                       maestroRepositoryAdapter.consultarHorarioTurnoById(idHorarioTurno),
                       citaRepository.findCitaDataSedeByIdRegional(idRegional),
                       citaRepository.findCitasByTurnoRegionalProfesional(
                               fechaTurno,idHorarioTurno,idRegional,idProfesional,EstadosCita.CANCELADA.getEstado())
                               .collectList())

                       .flatMapMany(tuple-> {
                           tuple.getT2().setFechaProgramada(LocalDateTime.of(fechaTurno,tuple.getT1().getHoraInicio()));
                           tuple.getT2().setIdProfesional(idProfesional);
                           tuple.getT2().setIdHorarioTurno(idHorarioTurno);
                           tuple.getT2().setIdRegional(idRegional);
                           tuple.getT3().add(0,tuple.getT2());
                           return Flux.fromIterable(tuple.getT3());
                       })

                       .buffer(Config.MAXSIZE,1)
                       .filter(citas -> citas.size() == Config.MAXSIZE)
                       .map(citas -> {
                           var duracionViaje = mapboxService.calcularTiempoViaje(
                                   new GeoUbicacion(citas.get(0).getLatitud(),citas.get(0).getLongitud()),
                                   new GeoUbicacion(citas.get(1).getLatitud(),citas.get(1).getLongitud())).block();

                          return  crearDesplazamientoData(citas.get(0),citas.get(1)).toBuilder()
                                     .tipo(TiposDesplazamientos.DVISITA.name())
                                     .duracion(duracionViaje)
                                     .holgura(Config.HOLGURA_DEFECTO)
                                     .build();
                          })
                          .flatMap(desplazamientoRepository::save)

                  .then(Mono.just(Boolean.TRUE)));
    }
}
