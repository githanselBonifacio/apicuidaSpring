package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface FototerapiaRepository extends ReactiveCrudRepository<FototerapiaData,Integer> {
    Flux<FototerapiaData> findByIdCita(String idCita);
}
