package co.com.sura.postgres.agenda.adapter;

import co.com.sura.autoagendador.models.AutoAgendador;
import co.com.sura.autoagendador.models.CitaGenetic;
import co.com.sura.autoagendador.models.Resultado;
import co.com.sura.agenda.gateway.AgendamientoAutomaticoRepository;
import co.com.sura.agenda.entity.TiposDesplazamientos;
import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.autoagendador.config.Config;
import co.com.sura.genericos.Numeros;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.maestros.repository.HorarioTurnoRepository;
import co.com.sura.postgres.maestros.repository.RegionalesRepository;
import co.com.sura.postgres.moviles.data.DesplazamientoData;
import co.com.sura.postgres.moviles.data.DesplazamientoRepository;
import co.com.sura.postgres.personal.data.ProfesionalData;
import co.com.sura.postgres.personal.repository.ProfesionalRepository;
import co.com.sura.mapbox.entity.GeoUbicacion;
import co.com.sura.mapbox.gateway.MapboxServiceRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;

import static co.com.sura.genericos.EstadosCita.AGENDADA;

@Repository
public class AgendamientoAutomaticoAdapter implements AgendamientoAutomaticoRepository {

    private final CitaRepository citaRepository;
    private final ProfesionalRepository profesionalRepository;
    private final DesplazamientoRepository desplazamientoRepository;

    private final RegionalesRepository regionalesRepository;

    private final HorarioTurnoRepository horarioTurnoRepository;
    private final MapboxServiceRepository mapboxService;
    private final AutoAgendador autoAgendador;

    @Autowired
    public AgendamientoAutomaticoAdapter(CitaRepository citaRepository, ProfesionalRepository profesionalRepository,
                                         DesplazamientoRepository desplazamientoRepository,
                                         RegionalesRepository regionalesRepository,
                                         HorarioTurnoRepository horarioTurnoRepository,
                                         MapboxServiceRepository mapboxService, AutoAgendador autoAgendador) {
        this.citaRepository = citaRepository;
        this.profesionalRepository = profesionalRepository;
        this.desplazamientoRepository = desplazamientoRepository;
        this.regionalesRepository = regionalesRepository;
        this.horarioTurnoRepository = horarioTurnoRepository;
        this.mapboxService = mapboxService;
        this.autoAgendador = autoAgendador;
    }
    @Override
    public Mono<Boolean> autoagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idRegional) {
        return this.desagendarTurnoCompleto(fechaTurno, idHorarioTurno, idRegional)
                .then(
                   Mono.zip(
                      this.citaRepository.findAllByTurnoRegionalHorario(
                              fechaTurno,idHorarioTurno,idRegional, EstadosCita.CANCELADA.getEstado()).collectList(),

                      this.profesionalRepository.findFromTurnoRegional(fechaTurno,idRegional,idHorarioTurno).collectList(),

                      buildCitaOrigenSede(fechaTurno,idRegional,idHorarioTurno))

                   .flatMap(tuple->{
                       this.autoAgendador.withCitas(ConverterAgenda.convertToListCitaGenetic(tuple.getT1()))
                               .andOrigen(tuple.getT3())
                               .andNumeroMoviles(tuple.getT2().size());

                       var mejorSolucion = autoAgendador.run();
                       this.autoAgendador.resetData();
                       return this.asignarListaCitaToProfesionalAutoagendar(tuple.getT2(),mejorSolucion);
                   }))
                .then(this.insertDesplazamientosAllCitasByProfesional(fechaTurno, idRegional, idHorarioTurno))
                .onErrorResume(e-> this.desagendarTurnoCompleto(fechaTurno, idHorarioTurno, idRegional)
                        .then(this.desplazamientoRepository.deleteAllByFechaTurno(fechaTurno,idHorarioTurno,idRegional))
                        .then(Mono.error(e)))
                .then(Mono.just(Boolean.TRUE));
    }


    @Override
    public Flux<Desplazamiento> consultarDesplazamientoByCitaPartida(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idRegional){
        return this.desplazamientoRepository.findByFechaProgramada(fechaProgramada,idRegional,idHorarioTurno)
                .map(ConverterAgenda :: converToDesplazamiento);
    }

    @Override
    public Mono<Boolean> desagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idRegional) {
        return this.desplazamientoRepository.deleteAllByFechaTurno(fechaTurno,idHorarioTurno,idRegional)
               .then(this.citaRepository
                    .desagendarTurnoCompleto(fechaTurno,idHorarioTurno,idRegional,EstadosCita.SIN_AGENDAR.getEstado()))
               .then(Mono.just(Boolean.TRUE));
    }

    @Override
    public Mono<Boolean> insertDesplazamientoCitaByProfesional(
            LocalDate fechaTurno, Integer idHorarioTurno,String idRegional, String idProfesional) {

       return this.desplazamientoRepository.deleteByFechaTurnoProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional)
               .then(Mono.zip(
                       this.horarioTurnoRepository.findById(idHorarioTurno),
                       this.citaRepository.findSedeByIdRegional(idRegional),
                       this.citaRepository.findAllByTurnoRegionalProfesional(
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
                       .flatMap(citas -> mapboxService.calcularTiempoViaje(
                               new GeoUbicacion(citas.get(0).getLatitud(),citas.get(0).getLongitud()),
                               new GeoUbicacion(citas.get(1).getLatitud(),citas.get(1).getLongitud()))

                       .map(tiempoDesplazamiento->DesplazamientoData
                                      .crearDesplazamientoData(citas.get(0),citas.get(1))
                                      .toBuilder()
                                      .tipo(TiposDesplazamientos.DVISITA.name())
                                      .duracion(tiempoDesplazamiento)
                                      .holgura(Config.HOLGURA_DEFECTO)
                                      .build()))
                       .flatMap(desplazamientoRepository::save)
                  .then(Mono.just(Boolean.TRUE)));
    }
    private Mono<CitaGenetic> buildCitaOrigenSede(LocalDate fechaTurno, String idRegional, Integer idHorarioTurno){
        return Mono.zip(
                        regionalesRepository.findById(idRegional),
                        horarioTurnoRepository.findById(idHorarioTurno)
                                .filter(HorarioTurnoData::getEsHorarioBase))
                .map(tuple-> CitaGenetic.builder()
                        .idCita(tuple.getT1().getId())
                        .latitud(tuple.getT1().getLatitud())
                        .longitud(tuple.getT1().getLongitud())
                        .duracion(0)
                        .holgura(Numeros.NOVECIENTOS_SEGUNDOS)
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
                                .flatMap(profesionalData -> this.insertDesplazamientoCitaByProfesional(
                                        fechaTurno, idHorarioTurno, idRegional, profesionalData.getNumeroIdentificacion()))
                                .then());
    }
}
