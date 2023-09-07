package co.com.sura.entity.remision;

import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.NovedadRequest;
import co.com.sura.dto.remision.RemisionRequest;
import co.com.sura.entity.agenda.PacienteTratamientoCita;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;
import java.util.List;

public interface RemisionCrudRepository {
    Flux<Remision>                consultarRemisiones();
    Mono<Boolean>                 crearRemisionCita (RemisionRequest remisionRequest, List<CitaRequest> citas);
    Mono<DatosAtencionPaciente>   consultarDatosAtencionPacienteByIdRemision (String idRemision);
    Mono<Paciente>                consultarPacienteFromRemision (String idRemision);

    Flux<PacienteTratamientoCita> consultarAllPacienteWithMedicamentosToFarmacia();
    Mono<Void>                    notificarMedicamentosToFarmacia(List<PacienteTratamientoCita> tratamientoCitasList);

    Flux<RegistroHistorialRemision>consultarHistoricoRemision(String idRemision);
    Mono<RegistroHistorialRemision>consultarDatosRemision(String idRemision);

    Mono<Void>    actualizarRemisionPorNovedad(
                           RemisionRequest remisionRequest, List<CitaRequest> citas, NovedadRequest novedadRequest);

    Mono<Boolean> egresarRemisionById(String idRemision);
}
