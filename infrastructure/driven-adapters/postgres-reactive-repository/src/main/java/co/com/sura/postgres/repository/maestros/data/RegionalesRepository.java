package co.com.sura.postgres.repository.maestros.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface RegionalesRepository extends ReactiveCrudRepository<RegionalesData,String> {

    Flux<RegionalesData> findAll();
    Mono<RegionalesData> findById(String idRegional);

}
