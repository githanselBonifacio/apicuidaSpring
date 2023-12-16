package co.com.sura.web.maestro;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.maestros.entity.EstadoCita;
import co.com.sura.maestros.entity.HorarioTurno;
import co.com.sura.maestros.entity.Profesion;
import co.com.sura.maestros.entity.Regional;
import co.com.sura.maestros.entity.TipoIdentificacion;
import co.com.sura.genericos.Response;
import co.com.sura.maestro.CrudMaestroUseCase;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

import java.util.List;

@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/maestros")
public class MaestroController {

    @Autowired
    private CrudMaestroUseCase crudMaestroUseCase;

    //ciudades
    @GetMapping("regionales")
    public Mono<Response<List<Regional>>> consultarRegional(){
        return crudMaestroUseCase.consultarCiudad()
                .collectList()
                .map(ciudades -> ResponseFactory.createStatus(
                        ciudades,
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

    @GetMapping("regionales/{idRegional}")
    public Mono<Response<Regional>> consultarRegionalById(@PathVariable String idRegional){
        return crudMaestroUseCase.consultarCiudadById(idRegional)
                .map(ciudades -> ResponseFactory.createStatus(
                        ciudades,
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
    @GetMapping("horarioTurno")
    public Mono<Response<List<HorarioTurno>>> consultarHorarioTurno(){
        return crudMaestroUseCase.consultarHorarioTurno()
                .collectList()
                .map(ciudades -> ResponseFactory.createStatus(
                        ciudades,
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

    // plan salud
    @GetMapping("estadosCita")
    public Mono<Response<List<EstadoCita>>> consultarPlanSalud(){
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
}
