package co.com.sura.postgres.repository.admin.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface CanalizacionRepository extends ReactiveCrudRepository<CanalizacionData,Integer> {

    Flux<CanalizacionData> findByIdCita(String idCita);
}
