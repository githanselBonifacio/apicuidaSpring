package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface SoporteNutricionalRepository extends ReactiveCrudRepository<SoporteNutricionalData,Integer> {
    Flux<SoporteNutricionalData> findByIdCita(String idCita);
}
