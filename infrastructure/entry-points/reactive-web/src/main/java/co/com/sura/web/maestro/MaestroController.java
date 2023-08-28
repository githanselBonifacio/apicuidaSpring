package co.com.sura.web.maestro;

import co.com.sura.entity.maestro.*;
import co.com.sura.maestro.CrudMaestroUseCase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/maestros")
public class MaestroController {

    @Autowired
    CrudMaestroUseCase crudMaestroUseCase;

    //ciudades
    @GetMapping("ciudades")
    public Flux<Ciudad> consultarCiudad(){return crudMaestroUseCase.consultarCiudad();}

    @GetMapping("ciudad/{idCiudad}")
    public Mono<Ciudad> consultarCiudadById(@PathVariable String idCiudad){
        return crudMaestroUseCase.consultarCiudadById(idCiudad);
    }
    //horario turno
    @GetMapping("horarioTurno")
    public Flux<HorarioTurno> consultarHorarioTurno(){return crudMaestroUseCase.consultarHorarioTurno();}

    @GetMapping("horarioTurno/{idHorarioTurno}")
    public Mono<HorarioTurno> consultarHorarioTurnoById(@PathVariable Integer idHorarioTurno){
        return crudMaestroUseCase.consultarHorarioTurnoById(idHorarioTurno);
    }

    // tipo Identificacion
    @GetMapping("tipoIdentificacion")
    public Flux<TipoIdentificacion> consultarTipoIdentificacion(){
        return crudMaestroUseCase.consultarTipoIdentificacion();
    }

    @GetMapping("tipoIdentificacion/{idTipoIdentificacion}")
    public Mono<TipoIdentificacion> consultarTipoIdentificacionById(@PathVariable Integer idTipoIdentificacion){
        return crudMaestroUseCase.consultarTipoIdentificacionById(idTipoIdentificacion);
    }

    // plan salud
    @GetMapping("estadosCita")
    public Flux<EstadoCita> consultarPlanSalud(){
        return crudMaestroUseCase.consultarEstadosCita();
    }
}
