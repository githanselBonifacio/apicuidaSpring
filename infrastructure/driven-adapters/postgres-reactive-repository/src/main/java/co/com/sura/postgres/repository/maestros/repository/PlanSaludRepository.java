package co.com.sura.postgres.repository.maestros.repository;

import co.com.sura.postgres.repository.maestros.data.PlanSaludData;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface PlanSaludRepository extends ReactiveCrudRepository<PlanSaludData,String> {
    Flux<PlanSaludData> findAll();
    Mono<PlanSaludData> findById(Integer idPlanSaludData);
}
