package co.com.sura.entity.personal;

import co.com.sura.dto.request.EliminarTurnoProfesionalRequest;
import co.com.sura.entity.remision.SecuenciaTurno;
import co.com.sura.genericos.ResultadoActualizacionTurno;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;

public interface PersonalRepository {
    //profesionales
    Flux<Profesional> consultarProfesionales();
    Flux<Profesional>                consultarProfesionalesByRegional(String idRegional);
    Mono<Profesional> crearProfesional(Profesional profesional);
    Mono<Profesional>                actualizarProfesional(Profesional profesional);

    //moviles
    Mono<Movil>                      crearMovil(Movil movil);
    Mono<Movil>                      actualizarMovil(Movil movil);
    Flux<Movil>                      consultarMoviles();
    Flux<Movil>                      consultarMovilesSinConductor();
    Flux<Movil>                      consultarMovilesByIdRegional(String idRegional);

    //conductores
    Mono<Conductor>                  crearConductor(Conductor profesional);
    Mono<Conductor>                  actualizarConductor(Conductor profesional);
    Flux<Conductor>                  consultarConductores();

    //turnos personal
    Flux<ProfesionalWithTurno>       consultarHorariosProfesionales(String fechaTurno, String idRegional);
    Flux<ResultadoActualizacionTurno>eliminarTurnosProfesionalesAccionMasiva(
                                             List<EliminarTurnoProfesionalRequest> request);

    Flux<ResultadoActualizacionTurno>asignarTurnosProfesionalesAccionMasiva(List<TurnoProfesional> request);
    Flux<Conductor>                  consultarHorariosConductores(LocalDate fechaTurno, String idRegional);
    Mono<Boolean>                    actualizarHorarioTurnoProfesionales(List<TurnoProfesional> turnos);

    //secuencia turno
    Flux<SecuenciaTurno>             consultarSecuencias();
    Mono<Boolean>                    configurarSecuenciaTurno(SecuenciaTurno secuenciaTurno);
}
