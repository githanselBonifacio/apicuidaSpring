package co.com.sura.web.agenda;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.agenda.entity.Actividad;
import co.com.sura.agenda.entity.Cita;
import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.personal.entity.Profesional;
import co.com.sura.personal.entity.TurnoProfesional;
import co.com.sura.remision.entity.procedimientos.Procedimientos;
import co.com.sura.remision.entity.datosremision.Tratamiento;
import co.com.sura.genericos.Response;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;
import java.time.Duration;
import java.time.LocalDate;
import java.util.List;


@RestController
@CrossOrigin(value = "http://localhost:4200")
@RequestMapping(value="/agenda")
public class AgendaController {
    private static final Integer TIMEOUT = 30;
    @Autowired
    private AgendaUseCase agendaUseCase;

    //profesionales en turno
    /**
     * consulta profesionales disponibles para asignar en turno
     * @param fechaTurno fecha del turno (LocalDate)
     * @param idRegional id regional (String)
     * @return profesionales disponibles para asignar en turno (Response<List<co.com.sura.personal.entity.Profesional.class>>)
     * */
    @GetMapping(value = "/profesionalesDisponiblesByTurnoRegional")
    public Mono<Response<List<Profesional>>> getProfesionalesDisponiblesByTurnoRegional(
             @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
             @RequestParam String idRegional){
        return agendaUseCase.consultarProfesionalesByTurnoRegional(fechaTurno, idRegional)
                .collectList()
                .map(profesionales -> ResponseFactory.createStatus(
                        profesionales,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }

    /**
     * consulta profesionales asignados en él en turno
     * @param fechaTurno fecha del turno (LocalDate)
     * @param idRegional id regional (String)
     * @param idHorarioTurno id horario turno (Integer)
     * @return profesionales asignados en turno (Response<List<co.com.sura.personal.entity.Profesional.class>>)
     * */
    @GetMapping(value = "/profesionalesFromTurnoRegional")
    public Mono<Response<List<Profesional>>> getProfesionalesfromTurnoRegional(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam String idRegional,
            @RequestParam Integer idHorarioTurno){
        return agendaUseCase.consultarProfesionalesFromTurnoRegional(fechaTurno, idRegional, idHorarioTurno)
                .collectList()
                .map(profesionales -> ResponseFactory.createStatus(
                        profesionales,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }
    /**
     * consulta todos los profesionales en una regional
     * @param idRegional id regional (String)
     * @return profesionales pertenecientes la regional (Response<List<co.com.sura.personal.entity.Profesional.class>>)
     * */
    @GetMapping(value = "/profesionales/{idRegional}")
    public Mono<Response<List<Profesional>>> getProfesionalesByRegional(@PathVariable String idRegional){
        return agendaUseCase.consultarProfesionalesByRegional(idRegional)
                .collectList()
                .map(actividades -> ResponseFactory.createStatus(
                        actividades,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }

    /**
     * inserta un registro a tabla turnoProfesional
     * @param turnoProfesional turno a insertar (co.com.sura.personal.entity.TurnoProfesional)
     * @return crea el turno (Response<Boolean>)
     * */
    @PostMapping(value = "/asignarProfesionalTurno")
    public Mono<Response<Boolean>> asignarProfesionalTurno(@RequestBody TurnoProfesional turnoProfesional){
        return agendaUseCase.asignarProfesionalTurno(turnoProfesional)
                .map(profesionales -> ResponseFactory.createStatus(
                        profesionales,
                        StatusCode.STATUS_200,
                        Mensajes.SE_ASIGNO_PROFESIONAL_TURNO.replace("?",turnoProfesional
                                .getFechaTurno().toString()),
                        Mensajes.SE_ASIGNO_PROFESIONAL_TURNO.replace("?",turnoProfesional
                                .getFechaTurno().toString()),
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.NO_ASIGNO_PROFESIONAL_TURNO,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }

    /**
     * elimina un registro a tabla turnoProfesional
     * @param turnoProfesional turno a eliminar (co.com.sura.personal.entity.TurnoProfesional)
     * @return elimina el turno (Response<Boolean>)
     * */
    @PostMapping(value = "/desasignarProfesionalTurno")
    public Mono<Response<Boolean>> desasignarProfesionalTurno(@RequestBody TurnoProfesional turnoProfesional){
        return agendaUseCase.desasignarProfesionalTurno(turnoProfesional)
                .map(profesionales -> ResponseFactory.createStatus(
                        profesionales,
                        StatusCode.STATUS_200,
                        Mensajes.SE_DESASIGNO_PROFESIONAL_TURNO.replace("?",turnoProfesional
                                .getFechaTurno().toString()),
                        Mensajes.SE_DESASIGNO_PROFESIONAL_TURNO.replace("?",turnoProfesional
                                .getFechaTurno().toString()),
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.NO_DESASIGNO_PROFESIONAL_TURNO,
                        e.getMessage(),
                        e.getMessage()
                )));
    }
    //turno

    /**
     * desagendar todas las citas en este turno
     * @param fechaTurno fecha del turno (LocalDate)
     * @param idRegional id regional (String)
     * @param idHorarioTurno id horario turno (Integer)
     * @return elimina el turno (Response<Boolean>)
     * @apiNote solo es posible ejecutar cuando todas las citas están en estado agendado,
     * si hay en estado progreso o finalizada no es posible desagendar de manera masiva
     * */
    @PutMapping(value = "/desagendarTurnoCompleto")
    public Mono<Response<Boolean>> desagendarTurnoCompleto(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional){
        return agendaUseCase.desagendarTurnoCompleto(fechaTurno, idHorarioTurno,idRegional)
                .map(seDesagendo -> ResponseFactory.createStatus(
                        seDesagendo,
                        StatusCode.STATUS_200,
                        Mensajes.TURNO_DESAGENDADO,
                        Mensajes.TURNO_DESAGENDADO,
                        Mensajes.PETICION_EXITOSA))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_TURNO_DESAGENDADO,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }
    /**
     * autoagendar todas las citas en este turno
     * @param fechaTurno fecha del turno (LocalDate)
     * @param idRegional id regional (String)
     * @param idHorarioTurno id horario turno (Integer)
     * @return elimina el turno (Response<Boolean>)
     * @apiNote si en el turno hay citas confirmadas, en progreso o finalizadas,
     * no se puede autoagendar con el algoritmo genético, se lanzará una exepción
     * */
    @PutMapping(value = "/autoagendarTurnoCompleto")
    public Mono<Response<Boolean>> autoagendarTurnoCompleto(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional
    ){
        return agendaUseCase.autoagendarTurnoCompleto(fechaTurno, idHorarioTurno, idRegional)
                .map(turnoAutoagendado -> ResponseFactory.createStatus(
                        turnoAutoagendado,
                        StatusCode.STATUS_200,
                        Mensajes.TURNO_AUTOAGENDADO,
                        Mensajes.TURNO_AUTOAGENDADO,
                        Mensajes.PETICION_EXITOSA))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_AUTOAGENDADO,
                        Mensajes.ERROR_AUTOAGENDADO,
                        e.getMessage())))
                .timeout(Duration.ofSeconds(TIMEOUT));
    }
   //actividades
    /**
     * consulta actividades asignadas en él en turno por profesional
     * @param fechaTurno fecha del turno (LocalDate)
     * @param idRegional id regional (String)
     * @param idHorarioTurno id horario turno (Integer)
     * @return actividades asignados en turno (Response<List<co.com.sura.agenda.entity.Actividad.class>>)
     * */
    @GetMapping(value = "/actividadesByprofesionalesRegionalHorario")
    public Mono<Response<List<Actividad>>> getActividadesByProfesionalesRegionalHorario(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional){
        return agendaUseCase.consultarActividadesProfesionalesRegionalHorario(fechaTurno,idHorarioTurno,idRegional)
                .collectList()
                .map(actividades -> ResponseFactory.createStatus(
                        actividades,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }
    /**
     * consulta desplazamientos asignados en él en turno
     * @param fechaTurno fecha del turno (LocalDate)
     * @param idRegional id regional (String)
     * @param idHorarioTurno id horario turno (Integer)
     * @return desplazamientos asignados en turno (Response<List<co.com.sura.moviles.entity.Desplazamiento.class>>)
     * */
    @GetMapping(value = "/desplazamientoVisita")
    public Mono<Response<List<Desplazamiento>>> getDesplazamientoByIdCitaPartida(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional ){
        return agendaUseCase.consultarDesplazamientoByTurnoRegional(fechaTurno,idHorarioTurno,idRegional)
                .collectList()
                .map(actividades -> ResponseFactory.createStatus(
                        actividades,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }
    /**
     * consulta citas en turno
     * @param fechaTurno fecha del turno (LocalDate)
     * @param idRegional id regional (String)
     * @param idHorarioTurno id horario turno (Integer)
     * @return citas en turno (Response<List<co.com.sura.agenda.entity.Cita.class>>)
     * */
    @GetMapping(value = "/citas")
    public Mono<Response<List<Cita>>> getCitasByTurnoRegional(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional) {
        return agendaUseCase.consultarCitasByTurnoRegional(fechaTurno,idHorarioTurno, idRegional)
                .collectList()
                .map(citas -> ResponseFactory.createStatus(
                        citas,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));

    }
    /**
     * reprogramar horario de cita
     * @param cita cita a reprogramar (co.com.sura.agenda.entity.Cita.class)
     * @return reprogramar cita (Response<Boolean>>)
     * @apiNote solo se modifica la hora programada en la fecha programada de la cita,
     * no se puede cambiar el día en la fecha
     * @apiNote se valida que la nueva hora no tenga cruce con otra cita y que este dentro del horario del turno
     * */
    @PutMapping(value = "/reprogramarCita")
    public Mono<Response<Boolean>> reprogramarCita(
          @RequestBody Cita cita) {
        return agendaUseCase.reprogramarCitaById(
                   cita.getFechaProgramada(),cita.getIdCita(),cita.getIdProfesional(),
                        cita.getFechaProgramada().toLocalDate(),cita.getIdHorarioTurno(), cita.getIdRegional())
                .map(seReprogramo -> ResponseFactory.createStatus(
                        seReprogramo,
                        StatusCode.STATUS_200,
                        Mensajes.SE_REPROGRAMO_HORA_CITA,
                        Mensajes.SE_REPROGRAMO_HORA_CITA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.NO_REPROGRAMO_HORA_CITA,
                        e.getMessage(),
                        e.getMessage()
                )));

    }
    /**
     * confirmar cita
     * @param idCita id de cita a confirmar (String)
     * @return estado confirmado (Response<Boolean>>)
     * @apiNote se confirma la cita cuando se actualiza el idEstado de la cita al correspondiente
     * @apiNote se valida que la cita este en estado agendado de lo contrario lanza una exepción
     * */
    @PutMapping(value = "/confirmarCita")
    public Mono<Response<Boolean>> confirmarCita(@RequestParam String idCita){
        return agendaUseCase.confirmarCita(idCita)
                .map(confirmada->ResponseFactory.createStatus(
                        confirmada,
                        StatusCode.STATUS_200,
                        Mensajes.ESTADO_CITA_ACTUALIZADO,
                        Mensajes.ESTADO_CITA_ACTUALIZADO,
                        Mensajes.ESTADO_CITA_ACTUALIZADO
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage(),
                        e.getMessage()
                )));
    }
    /**
     * confirmar una lista de citas en un turno
     * @param citas id de cita a confirmar (List<co.com.sura.agenda.entity.Cita>)
     * @return estado confirmado de las citas(Response<Boolean>>)
     * @apiNote se confirma la cita cuando se actualizan todos los idEstado de la cita al correspondiente
     * @apiNote solo se confirman las citas en estado agendadas, las demás citas no se les realiza cambios
     * */
    @PostMapping(value = "/confirmarCitasTurno")
    public Mono<Response<Boolean>> confirmarCitasTurno(@RequestBody List<Cita> citas){
        return agendaUseCase.confirmarCitasTurno(citas)
                .map(citasConfirmadas->ResponseFactory.createStatus(
                        Boolean.TRUE,
                        StatusCode.STATUS_200,
                        String.format(Mensajes.TURNO_CONFIRMADO,citasConfirmadas),
                        String.format(Mensajes.TURNO_CONFIRMADO,citasConfirmadas),
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_TURNO_CONFIRMADO,
                        e.getMessage(),
                        e.getMessage()
                )));
    }
    /**
     * iniciar atención cita, actualizar ha estado 'en progreso'
     * @param idCita id de cita a iniciar (String)
     * @return estado confirmado (Response<Boolean>>)
     * @apiNote se inicia atención de la cita cuando se actualiza el idEstado de la cita al correspondiente
     * @apiNote se valida que la cita este en estado confirmado de lo contrario lanza una exepción
     * */
    @PutMapping(value = "/iniciarAtencionCita")
    public Mono<Response<Boolean>> iniciarAtencionCita(@RequestParam String idCita){
        return agendaUseCase.iniciarAtencionCita(idCita)
                .map(confirmada->ResponseFactory.createStatus(
                        confirmada,
                        StatusCode.STATUS_200,
                        Mensajes.ESTADO_CITA_ACTUALIZADO,
                        Mensajes.ESTADO_CITA_ACTUALIZADO,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage(),
                        e.getMessage()
                )));
    }
    /**
     * finalizar cita , actualizar a estado 'finalizada'
     * @param idCita id de cita a finalizar (String)
     * @return estado confirmado (Response<Boolean>>)
     * @apiNote se finaliza la cita cuando se actualiza el idEstado de la cita al correspondiente
     * @apiNote se valida que la cita este en estado en progreso de lo contrario lanza una exepción
     * */
    @PutMapping(value = "/finalizarAtencionCita")
    public Mono<Response<Boolean>> finalizarAtencionCita(@RequestParam String idCita){
        return agendaUseCase.finalizarAtencionCita(idCita)
                .map(confirmada->ResponseFactory.createStatus(
                        confirmada,
                        StatusCode.STATUS_200,
                        Mensajes.ESTADO_CITA_ACTUALIZADO,
                        Mensajes.ESTADO_CITA_ACTUALIZADO,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage(),
                        e.getMessage()
                )));
    }
    /**
     * cancelar cita , actualizar a estado 'cancelada'
     * @param idCita id de cita a cancelar (String)
     * @return estado cancelado (Response<Boolean>>)
     * @apiNote se cancela la cita cuando se actualiza el idEstado de la cita al correspondiente
     * @apiNote se valida que la cita este en estado en sin agendar o agendada de lo contrario lanza una exepción
     * */
    @PutMapping(value = "cancelarCita")
    public Mono<Response<Boolean>> cancelarCita(
            @RequestParam String idCita,
            @RequestParam Integer idMotivoCancelacion){

        return agendaUseCase.cancelarCita(idCita,idMotivoCancelacion)
                .map(seCancelo->ResponseFactory.createStatus(
                        seCancelo,
                        StatusCode.STATUS_200,
                        Mensajes.SE_CANCELO_CITA,
                        Mensajes.SE_CANCELO_CITA,
                        Mensajes.PETICION_EXITOSA))
                .onErrorResume(e ->Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_CANCELAR_CITA,
                        e.getMessage(),
                        e.getMessage()
                )));
    }
    /**
     * asignar profesional a una cita
     * @param cita cita a asinar (co.com.sura.agenda.entity.Cita.class)
     * @return asignar profesional (Response<Boolean>>)
     * @apiNote la cita debe tener el idProfesional que se asignará a la cita
     * */
    @PutMapping(value = "/asignarProfesionalCita")
    public Mono<Response<Boolean>> asignarProfesionalCita(
            @RequestBody Cita cita){
        return agendaUseCase.asignarProfesionaCita(
                cita.getIdCita(),
                        cita.getIdProfesional(),
                        cita.getFechaProgramada(),
                        cita.getIdHorarioTurno() ,
                        cita.getIdRegional())
                .map(seAgendo->ResponseFactory.createStatus(
                        seAgendo,
                        StatusCode.STATUS_200,
                        Mensajes.SE_ASIGNO_PROFESIONAL_CITA,
                        Mensajes.SE_ASIGNO_PROFESIONAL_CITA,
                        Mensajes.PETICION_EXITOSA))
                .onErrorResume(e ->Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.NO_ASIGNO_PROFESIONAL_CITA,
                        e.getMessage(),
                        e.getMessage()
                )));


    }
    /**
     * desasignar profesional a una cita
     * @param idCita id de cita a desasinar (String)
     * @param idProfesional idProfesional que tiene asignado la cita (String)
     * @param fechaTurno fecha turno de la cita (LocalDate)
     * @param idRegional id regional del turno (String)
     * @param idHorarioTurno id horario del turno (Integer)
     * @return desasignar profesional (Response<Boolean>>)
     * @apiNote se recalculan nuevamente los desplazamientos del profeisonal en el turno con las citas restantes
     * */
    @PutMapping(value = "/desasignarProfesionalCita")
    public Mono<Response<Boolean>> desasignarProfesionalCita(
            @RequestParam String idCita,
            @RequestParam String idProfesional,
            @RequestParam ("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional) {
        return agendaUseCase.desasignarProfesionaCita(idCita, idProfesional, fechaTurno, idHorarioTurno, idRegional)
                .map(actividades -> ResponseFactory.createStatus(
                        actividades,
                        StatusCode.STATUS_200,
                        Mensajes.SE_DESASIGNO_PROFESIONAL_CITA,
                        Mensajes.SE_DESASIGNO_PROFESIONAL_CITA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.NO_DESASIGNO_PROFESIONAL_CITA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));

    }
    //tratamientos
    /**
     * consultar tratamientos en cita
     * @param idCita id de cita  (String)
     * @return lista de tratamientos (List<co.com.sura.remision.entity.datosremision.Tratamiento.class>)
     * */
    @GetMapping(value = "/tratamientos")
    public Mono<Response<List<Tratamiento>>> consultarTratamientosByCita(@RequestParam String idCita) {
        return agendaUseCase.consultarTratamientosByCita(idCita)
                .collectList()
                .map(tratamientos -> ResponseFactory.createStatus(
                        tratamientos,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));

    }
    /**
     * consultar procedimientos en cita
     * @param idCita id de cita  (String)
     * @return lista de tratamientos (List<co.com.sura.remision.entity.procedimientos.Procedimientos.class>)
     * */
    @GetMapping(value = "/procedimientos")
    public Mono<Response<Procedimientos>> consultarProcedimientosByIdCita(@RequestParam String idCita){
        return agendaUseCase.consultarProcedimietosByIdCita(idCita)
                .map(procedimientos -> ResponseFactory.createStatus(
                        procedimientos,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }

}
