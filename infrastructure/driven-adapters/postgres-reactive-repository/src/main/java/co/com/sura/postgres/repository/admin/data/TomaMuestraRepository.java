package co.com.sura.postgres.repository.admin.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface TomaMuestraRepository extends ReactiveCrudRepository<TomaMuestraData,Integer> {
    Flux<TomaMuestraData> findByIdCita(String idCita);
}
