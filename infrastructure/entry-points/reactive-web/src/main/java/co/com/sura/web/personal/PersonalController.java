package co.com.sura.web.personal;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.dto.request.EliminarTurnoProfesionalRequest;
import co.com.sura.entity.personal.Conductor;
import co.com.sura.entity.personal.Movil;
import co.com.sura.entity.personal.Profesional;
import co.com.sura.entity.personal.ProfesionalWithTurno;
import co.com.sura.entity.personal.TurnoProfesional;
import co.com.sura.entity.remision.SecuenciaTurno;
import co.com.sura.genericos.Response;
import co.com.sura.genericos.ResultadoActualizacionTurno;
import co.com.sura.personal.PersonalUseCase;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

import java.util.List;

@RestController
@CrossOrigin(origins = "*")
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
    @GetMapping(value = "/profesionales/{idRegional}")
    public Mono<Response<List<Profesional>>> getProfesionalesByRegional(@PathVariable String idRegional){
        return personalUseCase.consultarProfesionalByRegional(idRegional)
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
    @PostMapping(value = "/crearProfesional")
    public Mono<Response<Profesional>> crearProfesional(@RequestBody Profesional profesional) {
        return personalUseCase.crearProfesional(profesional)
                .map(profesionalCreado -> ResponseFactory.createStatus(
                        profesionalCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_CREA_PROFESIONAL.getValue(),
                        Mensajes.SE_CREA_PROFESIONAL.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_CREAR_PROFESIONAL.getValue(),
                        e.getMessage(),
                        e.getMessage()
                )));

    }

    @PutMapping(value = "/actualizarProfesional")
    public Mono<Response<Profesional>> actualizarProfesional(@RequestBody Profesional profesional) {
        return personalUseCase.actualizarProfesional(profesional)
                .map(profesionalCreado -> ResponseFactory.createStatus(
                        profesionalCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_ACTUALIZA_PROFESIONAL.getValue(),
                        Mensajes.SE_ACTUALIZA_PROFESIONAL.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_ACTTUALIZAR_PROFESIONAL.getValue(),
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
    @PostMapping(value = "/crearConductor")
    public Mono<Response<Conductor>> crearProfesional(@RequestBody Conductor conductor) {
        return personalUseCase.crearConductor(conductor)
                .map(conductorCreado -> ResponseFactory.createStatus(
                        conductorCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_CREA_CONDUCTOR.getValue(),
                        Mensajes.SE_CREA_CONDUCTOR.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_CREAR_CONDUCTOR.getValue(),
                        e.getMessage(),
                        e.getMessage()
                )));

    }

    @PutMapping(value = "/actualizarConductor")
    public Mono<Response<Conductor>> actualizarProfesional(@RequestBody Conductor conductor) {
        return personalUseCase.actualizarConductor(conductor)
                .map(conductorCreado -> ResponseFactory.createStatus(
                        conductorCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_ACTUALIZA_CONDUCTOR.getValue(),
                        Mensajes.SE_ACTUALIZA_CONDUCTOR.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_ACTUALIZAR_CONDUCTOR.getValue(),
                        e.getMessage(),
                        e.getMessage()
                )));

    }
    //moviles
    @PostMapping(value = "/crearMovil")
    public Mono<Response<Movil>> crearProfesional(@RequestBody Movil movil) {
        return personalUseCase.crearMovil(movil)
                .map(conductorCreado -> ResponseFactory.createStatus(
                        conductorCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_CREA_MOVIL.getValue(),
                        Mensajes.SE_CREA_MOVIL.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_CREAR_MOVIL.getValue(),
                        e.getMessage(),
                        e.getMessage()
                )));

    }

    @PutMapping(value = "/actualizarMovil")
    public Mono<Response<Movil>> actualizarMovil(@RequestBody Movil movil) {
        return personalUseCase.actualizarMovil(movil)
                .map(movilCreado -> ResponseFactory.createStatus(
                        movilCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_ACTUALIZA_MOVIL.getValue(),
                        Mensajes.SE_ACTUALIZA_MOVIL.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_ACTUALIZAR_MOVIL.getValue(),
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
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    @GetMapping(value = "movilesSinConductor")
    public Mono<Response<List<Movil>>> consultarMovilesSinConductor(){
        return personalUseCase.consultarMovilesSinConductor()
                .collectList()
                .map(movil->ResponseFactory.createStatus(
                        movil,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    @GetMapping(value = "/movilesByRegional/{idRegional}")
    public Mono<Response<List<Movil>>> consultarMovilesByIdRegional(@PathVariable String idRegional){
        return personalUseCase.consultarMovilesByIdRegional(idRegional)
                .collectList()
                .map(movil->ResponseFactory.createStatus(
                        movil,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
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
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
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
    @PostMapping("asignarTurnosProfesionalesAccionMasiva")
    public Mono<Response<List<ResultadoActualizacionTurno>>> asignarTurnosProfesionalesAccionMasiva(
            @RequestBody List<TurnoProfesional> request){
        return personalUseCase.asignarTurnosProfesionalAccionMasiva(request)
                .collectList()
                .map(resultado -> ResponseFactory.createStatus(
                        resultado,
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
    @PostMapping("secuenciasTurno")
    public Mono<Response<Boolean>> configurarSecuenciaTurno(@RequestBody SecuenciaTurno secuenciaTurno){
        return personalUseCase.configurarSecuenciaTurno(secuenciaTurno)
                .map(secuenciaCreada -> ResponseFactory.createStatus(
                        secuenciaCreada,
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
    //horarios de profesionales
    @GetMapping(value = "horarioTurno")
    public Mono<Response<List<ProfesionalWithTurno>>> consultarProfesionalesTurnos(
            @RequestParam("fechaTurno") String fechaTurno,
            @RequestParam String idRegional){
        return personalUseCase.consultarProfesionalesTurnoByFechaTurnoIdRegional(fechaTurno,idRegional)
                .collectList()
                .map(historial -> ResponseFactory.createStatus(
                        historial,
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

    @PutMapping(value = "/actualizarTurnoProfesional")
    public Mono<Response<Boolean>> actualizarTurnoProfesional( @RequestBody List<TurnoProfesional> turnosProfesional){
        return personalUseCase.actualizarTurnosByProfesional(turnosProfesional)
                .map(turnosCreado-> ResponseFactory.createStatus(
                        turnosCreado,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_ACTUALIZA_TURNO_PROFESIONAL.getValue(),
                        Mensajes.SE_ACTUALIZA_TURNO_PROFESIONAL.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.ERROR_ACTUALIZAR_TURNO_PROFESIONAL.getValue(),
                        Mensajes.ERROR_ACTUALIZAR_TURNO_PROFESIONAL.getValue(),
                        e.getMessage()
                )));
    }
}
