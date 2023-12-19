package co.com.sura.maestros.gateway;

import co.com.sura.maestros.entity.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface MaestroRepository {

    //regional
    Flux<Regional> consultarRegional();
    Mono<Regional> consultarRegionalById(String idRegional);

    //horario turno
    Flux<HorarioTurno> consultarHorarioTurno();
    Mono<HorarioTurno> consultarHorarioTurnoById(Integer idHorarioTurno);

    //tipo identificacion
    Flux<TipoIdentificacion> consultarTipoIdentificacion();
    Mono<TipoIdentificacion> consultarTipoIdentificacionById(Integer idTipoIdentificacion);

    //estado citas
    Flux<EstadoCita> consultarEstadosCita();

    //profesiones
    Flux<Profesion> consultarProfesiones();

}
