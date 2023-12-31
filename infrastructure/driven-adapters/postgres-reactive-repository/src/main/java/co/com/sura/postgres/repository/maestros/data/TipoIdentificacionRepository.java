package co.com.sura.postgres.repository.maestros.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface TipoIdentificacionRepository extends ReactiveCrudRepository<TipoIdentificacionData,String> {

    Flux<TipoIdentificacionData> findAll();
    Mono<TipoIdentificacionData> findById(Integer idTipoIdentificacion);
}
