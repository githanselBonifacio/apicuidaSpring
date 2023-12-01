package co.com.sura.farmacia;

import co.com.sura.entity.agenda.PacienteTratamientoCita;
import co.com.sura.entity.farmacia.FarmaciaRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;

public class FarmaciaUseCase {
    private final FarmaciaRepository farmaciaRepository;

    public FarmaciaUseCase(FarmaciaRepository farmaciaRepository) {
        this.farmaciaRepository = farmaciaRepository;
    }

    //farmacia
    public Flux<PacienteTratamientoCita> consultarAllTratamientosToFarmacia(){
        return farmaciaRepository.consultarAllPacienteWithMedicamentosToFarmacia();
    }
    public Flux<PacienteTratamientoCita> consultarAllTratamientosToFarmaciaWithFilter(
            LocalDate fechaTurno, Integer idHorario, String idRegional){
        return farmaciaRepository.consultarAllPacienteWithMedicamentosToFarmaciaByFilter(
                fechaTurno,idHorario,idRegional);
    }
    public Mono<Boolean> notificarMedicamentosToFarmacia (List<PacienteTratamientoCita> tratamientoCitasList){
        return farmaciaRepository.notificarMedicamentosToFarmacia(tratamientoCitasList);
    }
}
