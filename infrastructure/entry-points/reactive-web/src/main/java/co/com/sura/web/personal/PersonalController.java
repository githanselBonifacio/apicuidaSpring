package co.com.sura.web.personal;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.remision.dto.EliminarTurnoProfesionalRequest;
import co.com.sura.personal.entity.Conductor;
import co.com.sura.moviles.entity.Movil;
import co.com.sura.personal.entity.Profesional;
import co.com.sura.personal.entity.ProfesionalWithTurno;
import co.com.sura.personal.entity.TurnoProfesional;
import co.com.sura.personal.entity.SecuenciaTurno;
import co.com.sura.genericos.Response;
import co.com.sura.genericos.ResultadoActualizacionTurno;
import co.com.sura.personal.PersonalUseCase;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import java.util.List;

@RestController
@CrossOrigin(value = "http://localhost:4200")
@RequestMapping("/personal")
public class PersonalController {

    @Autowired
    private PersonalUseCase personalUseCase;

    //profesionales
    @GetMapping(value = "/profesionales")
    public Mono<Response<List<Profesional>>> getProfesionales(){
        return personalUseCase.consultarProfesional()
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
        return personalUseCase.consultarProfesionalByRegional(idRegional)
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
    @PostMapping(value = "/crearProfesional")
    public Mono<Response<Profesional>> crearConductor(@RequestBody Profesional profesional) {
        return personalUseCase.crearProfesional(profesional)
                .map(profesionalCreado -> ResponseFactory.createStatus(
                        profesionalCreado,
                        StatusCode.STATUS_200,
                        Mensajes.SE_CREA_PROFESIONAL,
                        Mensajes.SE_CREA_PROFESIONAL,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_CREAR_PROFESIONAL,
                        e.getMessage(),
                        e.getMessage()
                )));

    }

    @PutMapping(value = "/actualizarProfesional")
    public Mono<Response<Profesional>> actualizarConductor(@RequestBody Profesional profesional) {
        return personalUseCase.actualizarProfesional(profesional)
                .map(profesionalCreado -> ResponseFactory.createStatus(
                        profesionalCreado,
                        StatusCode.STATUS_200,
                        Mensajes.SE_ACTUALIZA_PROFESIONAL,
                        Mensajes.SE_ACTUALIZA_PROFESIONAL,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_ACTTUALIZAR_PROFESIONAL,
                        e.getMessage(),
                        e.getMessage()
                )));

    }
    //conductores
    @GetMapping(value = "/conductores")
    public Mono<Response<List<Conductor>>> getConductores(){
        return personalUseCase.consultarConductores()
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
    @PostMapping(value = "/crearConductor")
    public Mono<Response<Conductor>> crearConductor(@RequestBody Conductor conductor) {
        return personalUseCase.crearConductor(conductor)
                .map(conductorCreado -> ResponseFactory.createStatus(
                        conductorCreado,
                        StatusCode.STATUS_200,
                        Mensajes.SE_CREA_CONDUCTOR,
                        Mensajes.SE_CREA_CONDUCTOR,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_CREAR_CONDUCTOR,
                        e.getMessage(),
                        e.getMessage()
                )));

    }

    @PutMapping(value = "/actualizarConductor")
    public Mono<Response<Conductor>> actualizarConductor(@RequestBody Conductor conductor) {
        return personalUseCase.actualizarConductor(conductor)
                .map(conductorCreado -> ResponseFactory.createStatus(
                        conductorCreado,
                        StatusCode.STATUS_200,
                        Mensajes.SE_ACTUALIZA_CONDUCTOR,
                        Mensajes.SE_ACTUALIZA_CONDUCTOR,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_ACTUALIZAR_CONDUCTOR,
                        e.getMessage(),
                        e.getMessage()
                )));

    }
    //moviles
    @PostMapping(value = "/crearMovil")
    public Mono<Response<Movil>> crearMovil(@RequestBody Movil movil) {
        return personalUseCase.crearMovil(movil)
                .map(conductorCreado -> ResponseFactory.createStatus(
                        conductorCreado,
                        StatusCode.STATUS_200,
                        Mensajes.SE_CREA_MOVIL,
                        Mensajes.SE_CREA_MOVIL,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_CREAR_MOVIL,
                        e.getMessage(),
                        e.getMessage()
                )));

    }

    @PutMapping(value = "/actualizarMovil")
    public Mono<Response<Movil>> actualizarMovil(@RequestBody Movil movil) {
        return personalUseCase.actualizarMovil(movil)
                .map(movilCreado -> ResponseFactory.createStatus(
                        movilCreado,
                        StatusCode.STATUS_200,
                        Mensajes.SE_ACTUALIZA_MOVIL,
                        Mensajes.SE_ACTUALIZA_MOVIL,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_ACTUALIZAR_MOVIL,
                        e.getMessage(),
                        e.getMessage()
                )));

    }
    @GetMapping(value = "/moviles")
    public Mono<Response<List<Movil>>> consultarMoviles(){
        return personalUseCase.consultarMoviles()
                .collectList()
                .map(movil->ResponseFactory.createStatus(
                        movil,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }
    @GetMapping(value = "movilesSinConductor")
    public Mono<Response<List<Movil>>> consultarMovilesSinConductor(){
        return personalUseCase.consultarMovilesSinConductor()
                .collectList()
                .map(movil->ResponseFactory.createStatus(
                        movil,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }
    @GetMapping(value = "/movilesByRegional/{idRegional}")
    public Mono<Response<List<Movil>>> consultarMovilesByIdRegional(@PathVariable String idRegional){
        return personalUseCase.consultarMovilesByIdRegional(idRegional)
                .collectList()
                .map(movil->ResponseFactory.createStatus(
                        movil,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }

    //secuencia de turnos
    @GetMapping("secuenciasTurno")
    public  Mono<Response<List<SecuenciaTurno>>> consultarSecuenciasTurno(){
        return personalUseCase.consultarSecuenciasTurno()
                .collectList()
                .map(secuenciaTurno -> ResponseFactory.createStatus(
                        secuenciaTurno,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
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

    @PostMapping("eliminarTurnosProfesionalesAccionMasiva")
    public Mono<Response<List<ResultadoActualizacionTurno>>> eliminarTurnosProfesionalesAccionMasiva(
            @RequestBody List<EliminarTurnoProfesionalRequest> request){
        return personalUseCase.eliminarTurnosProfesionalAccionMasiva(request)
                .collectList()
                .map(resultado -> ResponseFactory.createStatus(
                        resultado,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
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
    @PostMapping("asignarTurnosProfesionalesAccionMasiva")
    public Mono<Response<List<ResultadoActualizacionTurno>>> asignarTurnosProfesionalesAccionMasiva(
            @RequestBody List<TurnoProfesional> request){
        return personalUseCase.asignarTurnosProfesionalAccionMasiva(request)
                .collectList()
                .map(resultado -> ResponseFactory.createStatus(
                        resultado,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
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
    @PostMapping("secuenciasTurno")
    public Mono<Response<Boolean>> configurarSecuenciaTurno(@RequestBody SecuenciaTurno secuenciaTurno){
        return personalUseCase.configurarSecuenciaTurno(secuenciaTurno)
                .map(secuenciaCreada -> ResponseFactory.createStatus(
                        secuenciaCreada,
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
    //horarios de profesionales
    @GetMapping(value = "horarioTurno")
    public Mono<Response<List<ProfesionalWithTurno>>> consultarProfesionalesTurnos(
            @RequestParam("fechaTurno") String fechaTurno,
            @RequestParam String idRegional){
        return personalUseCase.consultarProfesionalesTurnoByFechaTurnoIdRegional(fechaTurno,idRegional)
                .collectList()
                .map(historial -> ResponseFactory.createStatus(
                        historial,
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

    @PutMapping(value = "/actualizarTurnoProfesional")
    public Mono<Response<Boolean>> actualizarTurnoProfesional( @RequestBody List<TurnoProfesional> turnosProfesional){
        return personalUseCase.actualizarTurnosByProfesional(turnosProfesional)
                .map(turnosCreado-> ResponseFactory.createStatus(
                        turnosCreado,
                        StatusCode.STATUS_200,
                        Mensajes.SE_ACTUALIZA_TURNO_PROFESIONAL,
                        Mensajes.SE_ACTUALIZA_TURNO_PROFESIONAL,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.ERROR_ACTUALIZAR_TURNO_PROFESIONAL,
                        Mensajes.ERROR_ACTUALIZAR_TURNO_PROFESIONAL,
                        e.getMessage()
                )));
    }
}
