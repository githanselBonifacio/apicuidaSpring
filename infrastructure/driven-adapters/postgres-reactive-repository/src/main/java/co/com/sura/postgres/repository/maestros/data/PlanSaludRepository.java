package co.com.sura.postgres.repository.maestros.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface PlanSaludRepository extends ReactiveCrudRepository<PlanSaludData,String> {
    Flux<PlanSaludData> findAll();
    Mono<PlanSaludData> findById(Integer idPlanSaludData);
}
