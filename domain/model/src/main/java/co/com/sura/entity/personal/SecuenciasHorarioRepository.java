package co.com.sura.entity.personal;

import co.com.sura.dto.request.EliminarTurnoProfesionalRequest;
import co.com.sura.genericos.ResultadoActualizacionTurno;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;

public interface SecuenciasHorarioRepository {

    //turnos personal
    Flux<ProfesionalWithTurno> consultarHorariosProfesionales(String fechaTurno, String idRegional);
    Flux<ResultadoActualizacionTurno>eliminarTurnosProfesionalesAccionMasiva(
            List<EliminarTurnoProfesionalRequest> request);

    Flux<ResultadoActualizacionTurno>asignarTurnosProfesionalesAccionMasiva(List<TurnoProfesional> request);
    Flux<Conductor>                  consultarHorariosConductores(LocalDate fechaTurno, String idRegional);
    Mono<Boolean> actualizarHorarioTurnoProfesionales(List<TurnoProfesional> turnos);

    //secuencia turno
    Flux<SecuenciaTurno>             consultarSecuencias();
    Mono<Boolean>                    configurarSecuenciaTurno(SecuenciaTurno secuenciaTurno);
}
