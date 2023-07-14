package co.com.sura.postgres.repository.maestros.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface CiudadRepository extends ReactiveCrudRepository<CiudadData,String> {

    Flux<CiudadData> findAll();
    Mono<CiudadData> findById(String idCiudad);

}
