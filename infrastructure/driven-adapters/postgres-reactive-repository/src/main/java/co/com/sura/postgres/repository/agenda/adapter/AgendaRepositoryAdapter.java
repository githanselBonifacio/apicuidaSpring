package co.com.sura.postgres.repository.agenda.adapter;

import co.com.sura.constantes.Mensajes;
import co.com.sura.entity.agenda.Actividad;
import co.com.sura.entity.agenda.AgendaRepository;
import co.com.sura.entity.agenda.Cita;
import co.com.sura.entity.agenda.ProcedimientosCitaRepository;
import co.com.sura.entity.agenda.Tarea;
import co.com.sura.entity.agenda.TiposTarea;
import co.com.sura.entity.moviles.Desplazamiento;
import co.com.sura.entity.personal.TurnoProfesional;
import co.com.sura.entity.remision.Procedimientos;
import co.com.sura.entity.remision.Tratamiento;
import co.com.sura.exception.ErrorEstadoCitaNoValido;
import co.com.sura.exception.ExceptionNegocio;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.genericos.Numeros;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.agenda.data.CitaRepository;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoRepository;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoData;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoRepository;
import co.com.sura.postgres.repository.remision.data.TratamientoRepository;
import co.com.sura.postgres.repository.personal.data.ProfesionalData;
import co.com.sura.postgres.repository.personal.data.TurnoProfesionalesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Repository
public class AgendaRepositoryAdapter implements AgendaRepository {

    private final TurnoProfesionalesRepository turnoProfesionalesRepository;
    private final CitaRepository citaRepository;
    private final DesplazamientoRepository desplazamientoRepository;
    private final TratamientoRepository tratamientoRepository;
    private final ProcedimientosCitaRepository procedimientosCitaRepository;
    private final HorarioTurnoRepository horarioTurnoRepository;
    private final AgendamientoAutomaticoAdapter agendamientoAdapter;

    @Autowired
    public AgendaRepositoryAdapter(
            TurnoProfesionalesRepository turnoProfesionalesRepository, CitaRepository citaRepository,
            DesplazamientoRepository desplazamientoRepository, TratamientoRepository tratamientoRepository,
            ProcedimientosCitaRepository procedimientosCitaRepository,
            HorarioTurnoRepository horarioTurnoRepository, AgendamientoAutomaticoAdapter agendamientoAdapter) {


        this.turnoProfesionalesRepository = turnoProfesionalesRepository;
        this.citaRepository = citaRepository;
        this.desplazamientoRepository = desplazamientoRepository;
        this.tratamientoRepository = tratamientoRepository;
        this.procedimientosCitaRepository = procedimientosCitaRepository;
        this.horarioTurnoRepository = horarioTurnoRepository;
        this.agendamientoAdapter = agendamientoAdapter;
    }


    @Override
    public Mono<Boolean> asignarProfesionalTurno(TurnoProfesional turnoProfesional) {
        return  Mono.just(turnoProfesional)
                        .map(ConverterAgenda::converToTurnoProfesionalData)
                .flatMap(turnoProfesionalesRepository::save)
                .then(Mono.just(Boolean.TRUE));
    }

    @Override
    public Mono<Boolean> desasignarProfesionalTurno(TurnoProfesional turnoProfesional) {
        return citaRepository.findCitasByTurnoProfesional(
                turnoProfesional.getFechaTurno(),turnoProfesional.getIdProfesional())
                .filter(citaData -> !(citaData.getIdEstado() == EstadosCita.AGENDADA.getEstado() ||
                        citaData.getIdEstado() == EstadosCita.SIN_AGENDAR.getEstado()))
                .hasElements()
                .flatMap(validacion -> {
                    if(Boolean.TRUE.equals(validacion)){
                        return Mono.error(new ErrorEstadoCitaNoValido(
                               Mensajes.ERROR_TURNO_DESAGENDADO_ESTADOS_CITAS.getValue()));
                    }else{
                        return Mono.just(true);
                    }
                })
               .then(citaRepository.desagendarAllFromIdProfesional(
                        turnoProfesional.getFechaTurno(),
                        turnoProfesional.getIdHorarioTurno(),
                        turnoProfesional.getIdProfesional(),
                        EstadosCita.SIN_AGENDAR.getEstado()))

                .then(turnoProfesionalesRepository.deleteByFechaTurnoIdHorarioProfesional(
                        turnoProfesional.getFechaTurno(),
                        turnoProfesional.getIdHorarioTurno(),
                        turnoProfesional.getIdProfesional()))
                .then(desplazamientoRepository.deleteByFechaTurnoProfesional(
                        turnoProfesional.getFechaTurno(),
                        turnoProfesional.getIdHorarioTurno(),
                        turnoProfesional.getIdRegional(),
                        turnoProfesional.getIdProfesional()
                ))
                .then(Mono.just(Boolean.TRUE))
                .onErrorResume(Mono::error);
    }



    public Flux<Tarea> consultarTareasTurnoByProfesional(
            ProfesionalData profesionalData,
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional){

        return citaRepository
                .findCitasByTurnoRegionalProfesional(fechaTurno, idHorarioTurno, idRegional,
                        profesionalData.getNumeroIdentificacion(),EstadosCita.CANCELADA.getEstado())
                .map(ConverterAgenda :: convertToTarea)
                .map(tarea -> {
                    tarea.setTipo(TiposTarea.VISITA.name());
                    return tarea;
                })
                .mergeWith(desplazamientoRepository
                                .findAllByTurnoProfesional(
                                        fechaTurno,idHorarioTurno,idRegional,profesionalData.getNumeroIdentificacion())
                                .map(ConverterAgenda :: convertToTarea))
                .sort(Comparator.comparing(Tarea::getFechaProgramada));
    }

    @Override
    public Flux<Actividad> consultarActividadesByProfesionalesRegionalHorarioTurno(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional) {
         return turnoProfesionalesRepository.findTurnoProfesionalByCiudadHorario(
                 fechaTurno,idHorarioTurno,idRegional)
                 .flatMap(profesionalData -> {
                     Flux<Tarea> tareaFlux = consultarTareasTurnoByProfesional(
                             profesionalData, fechaTurno, idHorarioTurno, idRegional);
                     return tareaFlux.collectList().map(citas ->{
                                 List<Tarea> tareaList = new ArrayList<>(citas);
                                 return ConverterAgenda.convertToActividad(profesionalData)
                                         .toBuilder().tareas(tareaList).build();
                     });
                 })
                 .collectList()
                 .flatMapMany(actividades -> Flux.fromIterable(actividades)
                         .sort(Comparator.comparing(Actividad::getResponsable)));
    }


    @Override
    public Flux<Cita> consultarCitasByTurnoRegional(LocalDate fechaTurno, Integer idHorarioTurno, String idRegional) {
        return citaRepository.findCitasByTurnoRegionalHorario(
                fechaTurno, idHorarioTurno, idRegional,EstadosCita.CANCELADA.getEstado())
                .sort(Comparator.comparing(Cita::getIdEstado));
    }

    private Mono<Boolean> validarDisponibilidadFechaCita(
            CitaData cita , LocalDateTime fechaProgramada, String idProfesional){
      return Mono.just(cita)
         .flatMap(citaData -> Mono.zip(
              citaRepository.findCitaMasCercanaAnterior(
                               citaData.getFechaProgramada(),citaData.getIdCita(),
                               citaData.getIdHorarioTurno(),citaData.getIdRegional(), idProfesional)
                               .defaultIfEmpty(CitaData.builder().build()),

              citaRepository.findCitaMasCercanaPosterior(
                               citaData.getFechaProgramada().plusSeconds(citaData.getDuracion()), citaData.getIdCita(),
                               citaData.getIdHorarioTurno(),citaData.getIdRegional(), idProfesional)
                               .defaultIfEmpty(CitaData.builder().build()),

                               Mono.just(citaData)))

          .flatMap(citasTuple-> Mono.zip(
              desplazamientoRepository.findByIdCitaPartida(citasTuple.getT1().getIdCita())
                .defaultIfEmpty(DesplazamientoData.builder().duracion(0).build()),

              desplazamientoRepository.findByIdCitaPartida(citasTuple.getT3().getIdCita())
                .defaultIfEmpty(DesplazamientoData.builder().duracion(Numeros.NOVECIENTOS_SEGUNDOS.getValue()).build()),

              desplazamientoRepository.findBySede(
                         fechaProgramada,cita.getIdRegional(), idProfesional, cita.getIdHorarioTurno())
                .defaultIfEmpty(DesplazamientoData.builder().build()))

          .map(despTuple-> CitaData.validarFechasToReprogramar(citasTuple.getT1(),citasTuple.getT2(),citasTuple.getT3(),
                                despTuple.getT1(),despTuple.getT2(),despTuple.getT3(),fechaProgramada)));
    }
    private Mono<Boolean> validarAgendamientoCitaEnHorarioTurno(CitaData cita, LocalDateTime fechaReprogramada){
        return Mono.just(cita)
                .flatMap(citaData -> horarioTurnoRepository.findById(cita.getIdHorarioTurno())
                   .map(horarioTurnoData ->
                            HorarioTurnoData
                                 .validarHorarioCita(fechaReprogramada,horarioTurnoData,cita.getDuracion())));
    }
    @Override
    public Mono<Boolean> reprogramarCitaFromProfesional(LocalDateTime fechaProgramada, String idCita,
                                                        String idProfesional, LocalDate fechaTurno,
                                                        Integer idHorarioTurno, String idRegional) {
        return citaRepository.findById(idCita)
                .switchIfEmpty(Mono.error(new ExceptionNegocio(Mensajes.CITA_NO_EXISTE.getValue())))
                .flatMap(citaData -> Mono.zip(
                        validarDisponibilidadFechaCita (citaData,fechaProgramada,idProfesional),
                        validarAgendamientoCitaEnHorarioTurno(citaData,fechaProgramada)))

                .flatMap(tupleValidacion -> {
                    if(Boolean.FALSE.equals(tupleValidacion.getT1())){
                       return Mono.error(new ExceptionNegocio(Mensajes.ERROR_FECHA_CITA.getValue()));

                   }else if(Boolean.FALSE.equals(tupleValidacion.getT2())) {
                       return Mono.error(new ExceptionNegocio(Mensajes.ERROR_FECHA_CITA_HORARIO.getValue()));

                   }else{
                       return updateFechaProgramadaCita(fechaProgramada,idProfesional,idCita,idRegional,idHorarioTurno);
                   }
                });
    }
    private Mono<Boolean> updateFechaProgramadaCita(LocalDateTime fechaProgramada, String idProfesional,
                                                    String idCita,String idRegional, Integer idHorarioTurno){

        return citaRepository.updateFechaProgramada(fechaProgramada,idCita)
                .then(agendamientoAdapter.insertDesplazamientoCitaByProfesional(
                        fechaProgramada.toLocalDate(),idHorarioTurno,idRegional,idProfesional));
    }

    @Override
    public Flux<Desplazamiento> consultarDesplazamientoByCitaPartida(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idRegional){
        return desplazamientoRepository.findByFechaProgramada(fechaProgramada,idRegional,idHorarioTurno)
                .map(ConverterAgenda :: converToDesplazamiento);
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
