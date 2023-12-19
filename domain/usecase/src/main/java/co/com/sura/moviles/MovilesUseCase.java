package co.com.sura.moviles;

import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.moviles.gateway.MovilRepository;
import reactor.core.publisher.Flux;

import java.time.LocalDate;

public class MovilesUseCase {

    private final MovilRepository movilRepository;

    public MovilesUseCase(MovilRepository movilRepository) {
        this.movilRepository = movilRepository;
    }
    public Flux<Desplazamiento> consultarDesplazamientoByTurnoRegional(
            LocalDate fechaProgramada, Integer idHorarioTurno, String idRegional){
        return movilRepository.consultarDesplazamientoRegional(
                fechaProgramada,idHorarioTurno,idRegional);
    }
}
