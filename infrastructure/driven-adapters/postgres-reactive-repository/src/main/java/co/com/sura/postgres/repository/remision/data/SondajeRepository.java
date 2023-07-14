package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface SondajeRepository extends ReactiveCrudRepository<SondajeData,Integer> {
    Flux<SondajeData> findByIdCita(String idCita);
}
