package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;

public interface RemisionDiagnosticoRepository extends ReactiveCrudRepository<RemisionDiagnosticoData,String> {


    @Override
    Mono<Void> deleteById(String idRemision);

}
