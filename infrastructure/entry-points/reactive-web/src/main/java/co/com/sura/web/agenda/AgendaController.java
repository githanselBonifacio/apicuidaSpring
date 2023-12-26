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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;
import java.time.Duration;
import java.time.LocalDate;
import java.util.List;


@RestController
@RequestMapping(value="/agenda")
public class AgendaController {
    private static final Integer TIMEOUT = 30;
    @Autowired
    private AgendaUseCase agendaUseCase;

    //profesionales en turno
    @GetMapping(value = "/profesionalesByTurnoRegional")
    public Mono<Response<List<Profesional>>> getProfesionalesbyTurnoRegional(
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
                        e.getMessage()
                )))
                .timeout(Duration.ofSeconds(TIMEOUT));
    }
   //actividades
    @GetMapping(value = "/actividadesByprofesionalesRegionalHorario")
    public Mono<Response<List<Actividad>>> getActividadesByProfesionalesCiudadHorario(
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
