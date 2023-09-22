package co.com.sura.web.maestro;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.entity.maestro.*;
import co.com.sura.genericos.Response;
import co.com.sura.maestro.CrudMaestroUseCase;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
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

    @GetMapping("regionales/{idRegional}")
    public Mono<Response<Regional>> consultarRegionalById(@PathVariable String idRegional){
        return crudMaestroUseCase.consultarCiudadById(idRegional)
                .map(ciudades -> ResponseFactory.createStatus(
                        ciudades,
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
    //horario turno
    @GetMapping("horarioTurno")
    public Mono<Response<List<HorarioTurno>>> consultarHorarioTurno(){
        return crudMaestroUseCase.consultarHorarioTurno()
                .collectList()
                .map(ciudades -> ResponseFactory.createStatus(
                        ciudades,
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

    @GetMapping("horarioTurno/{idHorarioTurno}")
    public Mono<Response<HorarioTurno>> consultarHorarioTurnoById(@PathVariable Integer idHorarioTurno){
        return crudMaestroUseCase.consultarHorarioTurnoById(idHorarioTurno)
                .map(horarioTurno -> ResponseFactory.createStatus(
                        horarioTurno,
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

    // tipo Identificacion
    @GetMapping("tipoIdentificacion")
    public Mono<Response<List<TipoIdentificacion>>> consultarTipoIdentificacion(){
        return crudMaestroUseCase.consultarTipoIdentificacion() .collectList()
                .map(tipoIdentificacion -> ResponseFactory.createStatus(
                        tipoIdentificacion,
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

    @GetMapping("tipoIdentificacion/{idTipoIdentificacion}")
    public Mono<Response<TipoIdentificacion>> consultarTipoIdentificacionById(
            @PathVariable Integer idTipoIdentificacion){
        return crudMaestroUseCase.consultarTipoIdentificacionById(idTipoIdentificacion)
                .map(tipoIdentificacion -> ResponseFactory.createStatus(
                        tipoIdentificacion,
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

    // plan salud
    @GetMapping("estadosCita")
    public Mono<Response<List<EstadoCita>>> consultarPlanSalud(){
        return crudMaestroUseCase.consultarEstadosCita()
                .collectList()
                .map(estadosCitas -> ResponseFactory.createStatus(
                        estadosCitas,
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
