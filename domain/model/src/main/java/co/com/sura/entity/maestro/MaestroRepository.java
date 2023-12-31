package co.com.sura.entity.maestro;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface MaestroRepository {

    //ciudad
    Flux<Ciudad> consultarCiudad();
    Mono<Ciudad> consultarCiudadById(String idCiudad);

    //horario turno
    Flux<HorarioTurno> consultarHorarioTurno();
    Mono<HorarioTurno> consultarHorarioTurnoById(Integer idHorarioTurno);

    //tipo identificacion
    Flux<TipoIdentificacion> consultarTipoIdentificacion();
    Mono<TipoIdentificacion> consultarTipoIdentificacionById(Integer idTipoIdentificacion);

    //plan salud
    Flux<PlanSalud> consultarPlanSalud();
    Mono<PlanSalud> consultarPlanSaludById(Integer idPlanSalud);

}
