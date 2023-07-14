package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface CuracionRepository extends ReactiveCrudRepository<CuracionData,Integer> {
    Flux<CuracionData> findByIdCita(String idCita);
}
