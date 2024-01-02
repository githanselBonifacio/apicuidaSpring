package co.com.sura.web.maestro;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.maestros.entity.*;
import co.com.sura.genericos.Response;
import co.com.sura.maestro.CrudMaestroUseCase;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import java.util.List;

@RestController
@CrossOrigin(value = "http://localhost:4200")
@RequestMapping("/maestros")
public class MaestroController {

    @Autowired
    private CrudMaestroUseCase crudMaestroUseCase;

    //regionales
    /**
     * consultar regionales
     * @return lista de tratamientos (List<co.com.sura.maestros.entity.Regional.class>)
     * */
    @GetMapping("regionales")
    public Mono<Response<List<Regional>>> consultarRegional(){
        return crudMaestroUseCase.consultarRegionales()
                .collectList()
                .map(regionales -> ResponseFactory.createStatus(
                        regionales,
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
     * consultar regionales by id
     * @param idRegional id regional buscada
     * @return lista de tratamientos (List<co.com.sura.maestros.entity.Regional.class>)
     * */
    @GetMapping("regionales/{idRegional}")
    public Mono<Response<Regional>> consultarRegionalById(@PathVariable String idRegional){
        return crudMaestroUseCase.consultarRegionalById(idRegional)
                .map(regionales -> ResponseFactory.createStatus(
                        regionales,
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
    //horario turno
    /**
     * consultar horarios de turnos
     * @return lista de tratamientos (List<co.com.sura.maestros.entity.HorarioTurno.class>)
     * */
    @GetMapping("horarioTurno")
    public Mono<Response<List<HorarioTurno>>> consultarHorarioTurno(){
        return crudMaestroUseCase.consultarHorarioTurno()
                .collectList()
                .map(regionales -> ResponseFactory.createStatus(
                        regionales,
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
     * consultar horarios de turnos by id
     * @param idHorarioTurno id horario consultado
     * @return lista de tratamientos (List<co.com.sura.maestros.entity.HorarioTurno.class>)
     * */
    @GetMapping("horarioTurno/{idHorarioTurno}")
    public Mono<Response<HorarioTurno>> consultarHorarioTurnoById(@PathVariable Integer idHorarioTurno){
        return crudMaestroUseCase.consultarHorarioTurnoById(idHorarioTurno)
                .map(horarioTurno -> ResponseFactory.createStatus(
                        horarioTurno,
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

    // tipo Identificacion
    /**
     * consultar tipos de identificacion
     * @return lista de tratamientos (List<co.com.sura.maestros.entity.TipoIdentificacion.class>)
     * */
    @GetMapping("tipoIdentificacion")
    public Mono<Response<List<TipoIdentificacion>>> consultarTipoIdentificacion(){
        return crudMaestroUseCase.consultarTipoIdentificacion() .collectList()
                .map(tipoIdentificacion -> ResponseFactory.createStatus(
                        tipoIdentificacion,
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
     * consultar tipos de identificacion
     * @param idTipoIdentificacion id tipo identificacion consultada
     * @return lista de tratamientos (List<co.com.sura.maestros.entity.TipoIdentificacion.class>)
     * */
    @GetMapping("tipoIdentificacion/{idTipoIdentificacion}")
    public Mono<Response<TipoIdentificacion>> consultarTipoIdentificacionById(
            @PathVariable Integer idTipoIdentificacion){
        return crudMaestroUseCase.consultarTipoIdentificacionById(idTipoIdentificacion)
                .map(tipoIdentificacion -> ResponseFactory.createStatus(
                        tipoIdentificacion,
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
     * consultar estados citas
     * @return lista de tratamientos (List<co.com.sura.maestros.entity.EstadoCita.class>)
     * */
    @GetMapping("estadosCita")
    public Mono<Response<List<EstadoCita>>> consultarEstadocita(){
        return crudMaestroUseCase.consultarEstadosCita()
                .collectList()
                .map(estadosCitas -> ResponseFactory.createStatus(
                        estadosCitas,
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

    //profesiones
    /**
     * consultar profesiones
     * @return lista de tratamientos (List<co.com.sura.maestros.entity.Profesion.class>)
     * */
    @GetMapping("profesiones")
    public Mono<Response<List<Profesion>>> consultarProfesiones(){
        return crudMaestroUseCase.consultarProfesiones()
                .collectList()
                .map(estadosCitas -> ResponseFactory.createStatus(
                        estadosCitas,
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
     * consultar motivos de cancelación de cita
     * @return lista de motivos cancelación citas (List<co.com.sura.maestros.entity.MotivoCancelacionCita.class>)
     * */
    @GetMapping("motivosCancelacionCita")
    public Mono<Response<List<MotivoCancelacionCita>>> consultarMotivosCancelacionCita(){
        return crudMaestroUseCase.consultarMotivosCancelacionCita()
                .collectList()
                .map(motivos -> ResponseFactory.createStatus(
                        motivos,
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
