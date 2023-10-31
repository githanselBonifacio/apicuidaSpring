package co.com.sura.entity.admin;

import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.NovedadRequest;
import co.com.sura.dto.remision.RemisionRequest;
import co.com.sura.dto.request.EliminarTurnoProfesionalRequest;
import co.com.sura.entity.agenda.*;
import co.com.sura.genericos.ResultadoActualizacionTurno;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;

public interface RemisionCrudRepository {
    //remisiones
    Flux<Remision>                   consultarRemisiones();
    Mono<Boolean>                    crearRemisionCita (RemisionRequest remisionRequest, List<CitaRequest> citas);
    Flux<RegistroHistorialRemision>  consultarHistoricoRemision(String idRemision);
    Mono<RegistroHistorialRemision>  consultarDatosRemision(String idRemision);

    Mono<Boolean>                    actualizarRemisionPorNovedad(
                                                    RemisionRequest remisionRequest,
                                                    List<CitaRequest> citas, NovedadRequest novedadRequest);

    Mono<Boolean>                    egresarRemisionById(String idRemision);

    //datos paciente
    Mono<DatosAtencionPaciente>      consultarDatosAtencionPacienteByIdRemision (String idRemision);
    Mono<Paciente>                   consultarPacienteFromRemision (String idRemision);

    //farmacia
    Flux<PacienteTratamientoCita>    consultarAllPacienteWithMedicamentosToFarmacia();
    Flux<PacienteTratamientoCita>    consultarAllPacienteWithMedicamentosToFarmaciaByFilter(
                                            LocalDate fechaTurno, Integer idHorario, String idRegional);

    Mono<Boolean>                    notificarMedicamentosToFarmacia(
                                        List<PacienteTratamientoCita> tratamientoCitasList);

    //profesionales
    Flux<Profesional>                consultarProfesionales();
    Flux<Profesional>                consultarProfesionalesByRegional(String idRegional);
    Mono<Profesional>                crearProfesional(Profesional profesional);
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
    Flux<ResultadoActualizacionTurno>                  eliminarTurnosProfesionalesAccionMasiva(
                                                 List<EliminarTurnoProfesionalRequest> request);

    Flux<ResultadoActualizacionTurno>                   asignarTurnosProfesionalesAccionMasiva(List<TurnoProfesional> request);
    Flux<Conductor>                  consultarHorariosConductores( LocalDate fechaTurno, String idRegional);
    Mono<Boolean>                    actualizarHorarioTurnoProfesionales(List<TurnoProfesional> turnos);

    //secuencia turno
    Flux<SecuenciaTurno>             consultarSecuencias();
    Mono<Boolean>                    configurarSecuenciaTurno(SecuenciaTurno secuenciaTurno);

}
