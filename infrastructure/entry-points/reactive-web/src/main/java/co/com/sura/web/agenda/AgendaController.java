package co.com.sura.web.agenda;

import co.com.sura.agenda.AgendaUseCase;
import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.entity.agenda.Actividad;
import co.com.sura.entity.agenda.Conductor;
import co.com.sura.entity.agenda.Movil;
import co.com.sura.entity.moviles.Desplazamiento;
import co.com.sura.entity.agenda.Profesional;
import co.com.sura.entity.admin.*;
import co.com.sura.genericos.Response;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;


@RestController
@CrossOrigin(origins = "*")
@RequestMapping(value="/agenda")
public class AgendaController {
    private static final Integer TIMEOUT = 30;
    @Autowired
    private AgendaUseCase agendaUseCase;

    //profesionales en turno
    @GetMapping(value = "/profesionalesByTurnoCiudad")
    public Mono<Response<List<Profesional>>> getProfesionalesbyTurnoCiudad(
             @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
             @RequestParam String idRegional){
        return agendaUseCase.consultarProfesionalesByTurnoCiudad(fechaTurno, idRegional)
                .collectList()
                .map(profesionales -> ResponseFactory.createStatus(
                        profesionales,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    @GetMapping(value = "/profesionalesFromTurnoCiudad")
    public Mono<Response<List<Profesional>>> getProfesionalesfromTurnoCiudad(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam String idRegional,
            @RequestParam Integer idHorarioTurno){
        return agendaUseCase.consultarProfesionalesFromTurnoCiudad(fechaTurno, idRegional, idHorarioTurno)
                .collectList()
                .map(profesionales -> ResponseFactory.createStatus(
                        profesionales,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    @GetMapping(value = "/profesionales/{idCiudad}")
    public Mono<Response<List<Profesional>>> getProfesionalesByCiudad(@PathVariable String idRegional){
        return agendaUseCase.consultarProfesionalesByCiudad(idRegional)
                .collectList()
                .map(actividades -> ResponseFactory.createStatus(
                        actividades,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }


    @GetMapping(value = "/asignarProfesionalTurno")
    public Mono<Response<Boolean>> asignarProfesionalTurno(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idProfesional){
        return agendaUseCase.asignarProfesionalTurno(fechaTurno, idHorarioTurno,idProfesional)
                .map(profesionales -> ResponseFactory.createStatus(
                        profesionales,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_ASIGNO_PROFESIONAL_TURNO.getValue().replace("?",fechaTurno.toString()),
                        Mensajes.SE_ASIGNO_PROFESIONAL_TURNO.getValue().replace("?",fechaTurno.toString()),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.NO_ASIGNO_PROFESIONAL_TURNO.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    @GetMapping(value = "/desasignarProfesionalTurno")
    public Mono<Response<Boolean>> desasignarProfesionalTurno(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idProfesional){
        return agendaUseCase.desasignarProfesionalTurno(fechaTurno, idHorarioTurno,idProfesional)
                .map(profesionales -> ResponseFactory.createStatus(
                        profesionales,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_DESASIGNO_PROFESIONAL_TURNO.getValue().replace("?",fechaTurno.toString()),
                        Mensajes.SE_DESASIGNO_PROFESIONAL_TURNO.getValue().replace("?",fechaTurno.toString()),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.NO_DESASIGNO_PROFESIONAL_TURNO.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    //turno
    @GetMapping(value = "/desagendarTurnoCompleto")
    public Mono<Response<Boolean>> desagendarTurnoCompleto(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional){
        return agendaUseCase.desagendarTurnoCompleto(fechaTurno, idHorarioTurno,idRegional)
                .map(seDesagendo -> ResponseFactory.createStatus(
                        seDesagendo,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.TURNO_DESAGENDADO.getValue(),
                        Mensajes.TURNO_DESAGENDADO.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_TURNO_DESAGENDADO.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    @GetMapping(value = "/autoagendarTurnoCompleto")
    public Mono<Response<Boolean>> autoagendarTurnoCompleto(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional
    ){
        return agendaUseCase.autoagendarTurnoCompleto(fechaTurno, idHorarioTurno, idRegional)
                .map(turnoAutoagendado -> ResponseFactory.createStatus(
                        turnoAutoagendado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.TURNO_AUTOAGENDADO.getValue(),
                        Mensajes.TURNO_AUTOAGENDADO.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_AUTOAGENDADO.getValue(),
                        Mensajes.ERROR_AUTOAGENDADO.getValue(),
                        e.getMessage()
                )))
                .timeout(Duration.ofSeconds(TIMEOUT));
    }
   //actividades
    @GetMapping(value = "/actividadesByprofesionalesCiudadHorario")
    public Mono<Response<List<Actividad>>> getActividadesByProfesionalesCiudadHorario(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional){
        return agendaUseCase.consultarActividadesProfesionalesCiudadHorario(fechaTurno,idHorarioTurno,idRegional)
                .collectList()
                .map(actividades -> ResponseFactory.createStatus(
                        actividades,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    @GetMapping(value = "/desplazamientoVisita")
    public Mono<Response<List<Desplazamiento>>> getDesplazamientoByIdCitaPartida(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional ){
        return agendaUseCase.consultarDesplazamientoByIdCitaPartida(fechaTurno,idHorarioTurno,idRegional)
                .collectList()
                .map(actividades -> ResponseFactory.createStatus(
                        actividades,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    @GetMapping(value = "/citas")
    public Mono<Response<List<Cita>>> getCitasByTurnoCiudad(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional) {
        return agendaUseCase.consultarCitasByTurnoCiudad(fechaTurno,idHorarioTurno, idRegional)
                .collectList()
                .map(citas -> ResponseFactory.createStatus(
                        citas,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));

    }
    @GetMapping(value = "/reprogramarCita")
    public Mono<Response<Boolean>> reprogramarCita(
            @RequestParam("fechaProgramada")  String fechaProgramada,
            @RequestParam String idCita,
            @RequestParam String nuevaHora,
            @RequestParam String idProfesional,
            @RequestParam ("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional) {

        var hora = nuevaHora.split(":");

        var formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
        var localDateTime = LocalDateTime.parse(fechaProgramada, formatter)
                .withHour(Integer.parseInt(hora[0])).withMinute(Integer.parseInt(hora[1]));

        return agendaUseCase.reprogramarCitaById(
                localDateTime,idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional)
                .map(actividades -> ResponseFactory.createStatus(
                        actividades,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_REPROGRAMO_HORA_CITA.getValue(),
                        Mensajes.SE_REPROGRAMO_HORA_CITA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.NO_REPROGRAMO_HORA_CITA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));

    }

    @GetMapping(value = "/asignarProfesionalCita")
    public Mono<Response<Boolean>> asignarProfesionalCita(
            @RequestParam String idCita,
            @RequestParam String idProfesional,
            @RequestParam ("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional
    ) {
        return agendaUseCase.asignarProfesionaCita(idCita, idProfesional, fechaTurno, idHorarioTurno, idRegional)
                .map(actividades -> ResponseFactory.createStatus(
                        actividades,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_ASIGNO_PROFESIONAL_CITA.getValue(),
                        Mensajes.SE_ASIGNO_PROFESIONAL_CITA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.NO_ASIGNO_PROFESIONAL_CITA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));

    }
    @GetMapping(value = "/desasignarProfesionalCita")
    public Mono<Response<Boolean>> desasignarProfesionalCita(
            @RequestParam String idCita,
            @RequestParam String idProfesional,
            @RequestParam ("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam Integer idHorarioTurno,
            @RequestParam String idRegional) {
        return agendaUseCase.desasignarProfesionaCita(idCita, idProfesional, fechaTurno, idHorarioTurno, idRegional)
                .map(actividades -> ResponseFactory.createStatus(
                        actividades,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_DESASIGNO_PROFESIONAL_CITA.getValue(),
                        Mensajes.SE_DESASIGNO_PROFESIONAL_CITA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.NO_DESASIGNO_PROFESIONAL_CITA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));

    }
    @GetMapping(value = "/tratamientos")
    public Mono<Response<List<Tratamiento>>> consultarTratamientosByCita(@RequestParam String idCita) {
        return agendaUseCase.consultarTratamientosByCita(idCita)
                .collectList()
                .map(tratamientos -> ResponseFactory.createStatus(
                        tratamientos,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));

    }

    @GetMapping(value = "/procedimientos")
    public Mono<Response<Procedimientos>> consultarProcedimientosByIdCita(@RequestParam String idCita){
        return agendaUseCase.consultarProcedimietosByIdCita(idCita)
                .map(procedimientos -> ResponseFactory.createStatus(
                        procedimientos,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

}
