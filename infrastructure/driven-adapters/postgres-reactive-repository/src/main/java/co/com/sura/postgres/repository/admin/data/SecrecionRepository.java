package co.com.sura.postgres.repository.admin.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface SecrecionRepository extends ReactiveCrudRepository<SecrecionData,Integer> {
    Flux<SecrecionData> findByIdCita(String idCita);
}
