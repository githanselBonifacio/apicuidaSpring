package co.com.sura.farmacia.gateway;

import co.com.sura.agenda.entity.PacienteTratamientoCita;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;

public interface FarmaciaRepository {
    //farmacia
    Flux<PacienteTratamientoCita> consultarAllPacienteWithMedicamentosToFarmacia();
    Flux<PacienteTratamientoCita>    consultarAllPacienteWithMedicamentosToFarmaciaByFilter(
            LocalDate fechaTurno, Integer idHorario, String idRegional);

    Mono<Boolean> notificarMedicamentosToFarmacia(
            List<PacienteTratamientoCita> tratamientoCitasList);
}
