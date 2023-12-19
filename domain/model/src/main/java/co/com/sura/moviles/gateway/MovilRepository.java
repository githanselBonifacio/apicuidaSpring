package co.com.sura.moviles.gateway;

import co.com.sura.moviles.entity.Desplazamiento;
import reactor.core.publisher.Flux;

import java.time.LocalDate;

public interface MovilRepository {
    Flux<Desplazamiento> consultarDesplazamientoRegional(
            LocalDate fechaProgramada,
            Integer idHorarioTurno,
            String idRegional);
}
