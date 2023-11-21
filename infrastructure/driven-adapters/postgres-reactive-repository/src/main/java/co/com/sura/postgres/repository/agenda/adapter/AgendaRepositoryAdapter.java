package co.com.sura.postgres.repository.agenda.adapter;

import co.com.sura.autoagendador.AutoAgendador;
import co.com.sura.autoagendador.OrigenRegional;
import co.com.sura.autoagendador.Resultado;
import co.com.sura.entity.agenda.Actividad;
import co.com.sura.entity.agenda.AgendaRepository;
import co.com.sura.entity.agenda.Cita;
import co.com.sura.entity.agenda.ProcedimientosCitaRepository;
import co.com.sura.entity.agenda.Tarea;
import co.com.sura.entity.moviles.Desplazamiento;
import co.com.sura.entity.personal.Profesional;
import co.com.sura.entity.personal.TurnoProfesional;
import co.com.sura.entity.remision.Procedimientos;
import co.com.sura.entity.remision.Tratamiento;
import co.com.sura.exception.ErrorEstadoNoValidoDesagendarCita;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.agenda.data.CitaRepository;
import co.com.sura.postgres.repository.remision.adapter.ConverterRemision;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoData;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoRepository;
import co.com.sura.postgres.repository.remision.data.TratamientoRepository;
import co.com.sura.postgres.repository.personal.data.ProfesionalData;
import co.com.sura.postgres.repository.personal.data.ProfesionalRepository;
import co.com.sura.postgres.repository.personal.data.TurnoProfesionalesRepository;
import co.com.sura.services.mapbox.GeoUbicacion;
import co.com.sura.services.mapbox.MapboxService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import static co.com.sura.autoagendador.IdRegional.getByIdCiudad;
import static co.com.sura.constantes.Mensajes.ERROR_TURNO_DESAGENDADO_ESTADOS_CITAS;
import static co.com.sura.postgres.repository.moviles.data.DesplazamientoData.crearDesplazamientoData;

@Repository
public class AgendaRepositoryAdapter implements AgendaRepository {

    private static final Integer MAXSIZE                       = 2;
    private static final Integer NUMERO_GENERACIONES           = 1000;
    private static final Integer SIZE_POBLACION_INICIAL        = 10;
    private static final Integer NUMERO_PADRES_EMPAREJADOS     = 5;
    private static final Integer HOLGURA_DEFECTO               = 1200;
    private static final double  PENALIZACION_HOLGURA_NEGATIVA = 1e6;
    private static final String DESPLAZAMIENTO_VISITA          = "dvisita";
    private static final String TIPO_TAREA_VISITA              = "visita";
    private final MapboxService mapboxService;
    private final TurnoProfesionalesRepository turnoProfesionalesRepository;
    private final ProfesionalRepository profesionalRepository;
    private final CitaRepository citaRepository;
    private final DesplazamientoRepository desplazamientoRepository;
    private final TratamientoRepository tratamientoRepository;
    private final ProcedimientosCitaRepository procedimientosCitaRepository;

    @Autowired
    public AgendaRepositoryAdapter(
            MapboxService mapboxService, TurnoProfesionalesRepository turnoProfesionalesRepository,
            ProfesionalRepository profesionalRepository, CitaRepository citaRepository,
            DesplazamientoRepository desplazamientoRepository, TratamientoRepository tratamientoRepository,
            ProcedimientosCitaRepository procedimientosCitaRepository) {

        this.mapboxService = mapboxService;
        this.turnoProfesionalesRepository = turnoProfesionalesRepository;
        this.profesionalRepository = profesionalRepository;
        this.citaRepository = citaRepository;
        this.desplazamientoRepository = desplazamientoRepository;
        this.tratamientoRepository = tratamientoRepository;
        this.procedimientosCitaRepository = procedimientosCitaRepository;
    }


    @Override
    public Flux<Profesional> consultarProfesionalByTurnoRegional(LocalDate fechaTurno, String idCiudad) {
        return profesionalRepository.findByTurnoRegional(fechaTurno,idCiudad)
                .map(ConverterRemision:: convertToProfesional);
    }

    @Override
    public Flux<Profesional> consultarProfesionalFromTurnoRegional(
            LocalDate fechaTurno, String idCiudad, Integer idHorarioTurno) {
        return profesionalRepository.findFromTurnoRegional(fechaTurno,idCiudad,idHorarioTurno)
                .map(ConverterRemision:: convertToProfesional);
    }

    @Override
    public Mono<Boolean> asignarProfesionalTurno(TurnoProfesional turnoProfesional) {
        return  turnoProfesionalesRepository.save(ConverterAgenda.converToTurnoProfesionalData(turnoProfesional))
                .then(Mono.just(Boolean.TRUE));
    }

    @Override
    public Mono<Boolean> desasignarProfesionalTurno(TurnoProfesional turnoProfesional) {
        return citaRepository.findCitasByTurnoProfesional(
                turnoProfesional.getFechaTurno(),turnoProfesional.getIdProfesional())
                .collectList()
                .map(citasData -> !citasData.stream()
                            .allMatch(cita -> cita.getIdEstado() == EstadosCita.AGENDADA.getEstado() ||
                            cita.getIdEstado() == EstadosCita.SIN_AGENDAR.getEstado()))
                .flatMap(validacion -> {
                    if(Boolean.TRUE.equals(validacion)){
                        return Mono.error(new ErrorEstadoNoValidoDesagendarCita(
                                ERROR_TURNO_DESAGENDADO_ESTADOS_CITAS.getValue()));
                    }else{
                        return Mono.just(true);
                    }
                })
                .then(citaRepository.desagendarAllFromIdProfesional(
                        turnoProfesional.getFechaTurno(),
                        turnoProfesional.getIdHorarioTurno(),
                        turnoProfesional.getIdProfesional()))

                .then(Mono.from(turnoProfesionalesRepository.deleteByFechaTurnoIdHorarioProfesional(
                        turnoProfesional.getFechaTurno(),
                        turnoProfesional.getIdHorarioTurno(),
                        turnoProfesional.getIdProfesional())))
                .then(Mono.just(Boolean.TRUE))
                .onErrorResume(Mono::error);
    }

    @Override
    public Flux<Profesional> consultarProfesionalesByIdRegional(String idRegional) {
        return profesionalRepository.findByIdRegional(idRegional)
                .map(ConverterRemision:: convertToProfesional);
    }

    public Flux<Tarea> consultarTareasTurnoByProfesional(
            ProfesionalData profesionalData,
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional){

        return citaRepository
                .findCitasByTurnoRegionalProfesional(fechaTurno, idHorarioTurno, idRegional,
                        profesionalData.getNumeroIdentificacion())
                .map(ConverterAgenda :: convertToTarea)
                .map(tarea -> {
                    tarea.setTipo(TIPO_TAREA_VISITA);
                    return tarea;
                })
                .mergeWith(
                        desplazamientoRepository
                                .findByIdCitaPartidaByProfesional(
                                        fechaTurno,idHorarioTurno,idRegional,profesionalData.getNumeroIdentificacion())
                                .map(ConverterAgenda :: convertToTarea)
                );
    }

    @Override
    public Flux<Actividad> consultarActividadesByProfesionalesCiudadHorarioTurno(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional) {
         return turnoProfesionalesRepository.findTurnoProfesionalByCiudadHorario(
                 fechaTurno,idHorarioTurno,idRegional)
                 .flatMap(profesionalData -> {
                     Flux<Tarea> tareaFlux = consultarTareasTurnoByProfesional(
                             profesionalData, fechaTurno, idHorarioTurno, idRegional
                     );
                     return tareaFlux.collectList().map(citas ->{
                                 List<Tarea> tareaList = new ArrayList<>(citas);
                                 return ConverterAgenda.convertToActividad(profesionalData)
                                         .toBuilder().tareas(tareaList).build();
                             });
                 })
                 .collectList()
                 .flatMapMany(actividades -> Flux.fromIterable(actividades)
                         .sort(Actividad::compareTo));
    }


    @Override
    public Flux<Cita> consultarCitasByTurnoRegional(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad) {
        return citaRepository.findCitasByTurnoRegionalHorario(fechaTurno, idHorarioTurno, idCiudad);
    }

    @Override
    public Mono<Boolean> reprogramarCita(LocalDateTime fechaProgramada, String idCita, String idProfesional,
                                         LocalDate fechaTurno, Integer idHorarioTurno, String idRegional) {
        return citaRepository.actualizarFechaProgramada(fechaProgramada,idCita)
                .then(calcularDesplazamientoCitaByProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional));
    }

    @Override
    public Mono<Boolean> agendarToProfesional(
            String idCita, String idProfesional,  LocalDate fechaTurno,Integer idHorarioTurno,String idRegional) {
        return citaRepository.agendarToProfesional(idCita,idProfesional)
                .then(calcularDesplazamientoCitaByProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional));
    }

    @Override
    public Mono<Boolean> desagendarToProfesional(
            String idCita,String idProfesional,LocalDate fechaTurno,Integer idHorarioTurno,String idRegional) {
        return citaRepository.desagendarToProfesional(idCita)
                .then(calcularDesplazamientoCitaByProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional));
    }

    @Override
    public Mono<Boolean> desagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad) {
        return desplazamientoRepository.deleteAllByFechaTurno(fechaTurno,idHorarioTurno,idCiudad)
                .then(citaRepository.desagendarTurnoCompleto(fechaTurno,idHorarioTurno,idCiudad))
                .then(Mono.just(Boolean.TRUE));
    }

    private Mono<Void> asignarListaCitaToProfesionalAutoagendar (
            List<ProfesionalData> profesionalesDataList, Resultado mejoResultado){
        return Flux.fromIterable(profesionalesDataList)
           .flatMap(profesionalData -> {
              int index = profesionalesDataList.indexOf(profesionalData);
              int sizeCitasGen = mejoResultado.getIndividuo().getCitaGen().get(index).size();
              return Flux.fromIterable(mejoResultado.getIndividuo().getCitaGen().get(index).subList(1, sizeCitasGen))
                            .flatMap(citaGenetic -> {
                                String idCita = citaGenetic.getIdCita();
                                String idProfesional = profesionalData.getNumeroIdentificacion();
                                return citaRepository.agendarToProfesional(idCita, idProfesional);
                            });
                }).then();
    }
    private Mono<Void> insertDesplazamientosAllCitasByProfesional(
            LocalDate fechaTurno, String idCiudad,Integer idHorarioTurno){

        return profesionalRepository.findFromTurnoRegional(fechaTurno,idCiudad,idHorarioTurno)
                .collectList()
                .flatMap(profesionalListData -> Flux.fromIterable(profesionalListData)
                        .flatMap(profesionalData -> calcularDesplazamientoCitaByProfesional(
                                fechaTurno,
                                idHorarioTurno,
                                idCiudad,
                                profesionalData.getNumeroIdentificacion()
                        )).then());
    }

    @Override
    public Mono<Boolean> autoagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad) {
      return desplazamientoRepository.deleteAllByFechaTurno(fechaTurno,idHorarioTurno,idCiudad)
         .then(citaRepository.findCitasByTurnoRegionalHorario(fechaTurno,idHorarioTurno,idCiudad)
         .collectList()
         .flatMap(citaDataList -> profesionalRepository.findFromTurnoRegional(fechaTurno,idCiudad,idHorarioTurno)
             .collectList().flatMap(profesionalesDataList -> {

               var autoAgendador = new AutoAgendador(
                       OrigenRegional.getOrigenCiudadById(getByIdCiudad(idCiudad)).getCitaGenetic(),
                  ConverterAgenda.convertToListCitaGenetic(citaDataList), profesionalesDataList.size(),
                  NUMERO_GENERACIONES, SIZE_POBLACION_INICIAL, NUMERO_PADRES_EMPAREJADOS, PENALIZACION_HOLGURA_NEGATIVA,
                        mapboxService);

               autoAgendador.run();
               return asignarListaCitaToProfesionalAutoagendar(profesionalesDataList,autoAgendador.mejorSolucion());
             })
         )).then(insertDesplazamientosAllCitasByProfesional(fechaTurno,idCiudad,idHorarioTurno))
              .then(Mono.just(Boolean.TRUE));
    }

    @Override
    public Flux<Desplazamiento> consultarDesplazamientoByCitaPartida(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idCiudad){
        return desplazamientoRepository.findByIdCitaPartida(fechaProgramada,idHorarioTurno,idCiudad)
                .map(ConverterAgenda :: converToDesplazamiento);
    }

    @Override
    public Mono<Boolean> calcularDesplazamientoCitaByProfesional(
            LocalDate fechaTurno, Integer idHorarioTurno,String idRegional, String idProfesional) {

        return desplazamientoRepository.deleteByFechaTurnoProfesional(fechaTurno,idHorarioTurno,idProfesional)
            .then(citaRepository.findCitasByTurnoRegionalProfesional(fechaTurno,idHorarioTurno,idRegional,idProfesional)
            .collectList()
            .flatMapMany(Flux::fromIterable)
            .buffer(MAXSIZE,1)
            .filter(citas -> citas.size() == MAXSIZE)
            .flatMap(
                   citas ->{
                      List<DesplazamientoData> desplazamientosList = new ArrayList<>();
                      CitaData citaPartida = citas.get(0);
                      CitaData citaDestino = citas.get(1);

                      var duracionViaje = mapboxService.calcularTiempoViaje(
                              new GeoUbicacion(citaPartida.getLatitud(),citaPartida.getLongitud()),
                              new GeoUbicacion(citaDestino.getLatitud(),citaDestino.getLongitud())).block();

                      var desplazamientoData = crearDesplazamientoData(citaPartida,citaDestino)
                            .toBuilder().tipo(DESPLAZAMIENTO_VISITA)
                            .idHorarioTurno(idHorarioTurno).duracion(duracionViaje).holgura(HOLGURA_DEFECTO).build();
                         desplazamientosList.add(desplazamientoData);

                         return desplazamientoRepository.saveAll(desplazamientosList);
                   }
               )
               .then(Mono.just(Boolean.TRUE))
        );
    }

    @Override
    public Flux<Tratamiento> consultarTratamientoByCitas(String idCita) {
        return tratamientoRepository.findByIdCita(idCita)
                .map(ConverterAgenda :: convertToTratamiento);
    }

    @Override
    public Mono<Procedimientos> consultarProcedimientosByIdCita(String idCita) {
        return procedimientosCitaRepository.consultarProcedimientosByIdCita(idCita);

    }

}
