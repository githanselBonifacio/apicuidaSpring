package co.com.sura.web.maestro;

import co.com.sura.entity.maestro.Ciudad;
import co.com.sura.entity.maestro.HorarioTurno;
import co.com.sura.entity.maestro.PlanSalud;
import co.com.sura.entity.maestro.TipoIdentificacion;
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
    @GetMapping("planSalud")
    public Flux<PlanSalud> consultarPlanSalud(){
        return crudMaestroUseCase.consultarPlanSalud();
    }

    @GetMapping("planSalud/{idPlanSalud}")
    public Mono<PlanSalud> consultarPlanSaludById(@PathVariable Integer idPlanSalud){
        return crudMaestroUseCase.consultarPlanSaludById(idPlanSalud);
    }
}
