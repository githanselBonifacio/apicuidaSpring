package co.com.sura.postgres.repository.maestros.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface HorarioTurnoRepository extends ReactiveCrudRepository<HorarioTurnoData,Integer> {
    Flux<HorarioTurnoData> findAll();
    Mono<HorarioTurnoData> findById(Integer idCiudad);
}
