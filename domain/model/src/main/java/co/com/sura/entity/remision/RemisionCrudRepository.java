package co.com.sura.entity.remision;

import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.NovedadRequest;
import co.com.sura.dto.remision.RemisionRequest;
import co.com.sura.entity.agenda.PacienteTratamientoCita;
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


}
