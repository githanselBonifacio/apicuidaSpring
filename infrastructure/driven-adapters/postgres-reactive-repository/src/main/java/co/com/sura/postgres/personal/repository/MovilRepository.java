package co.com.sura.postgres.personal.repository;

import co.com.sura.postgres.personal.data.MovilData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface MovilRepository extends ReactiveCrudRepository<MovilData,String> {
    Flux<MovilData> findByIdRegional(String idRegional);


   @Query("INSERT INTO public.moviles(matricula) VALUES ($1);")
   Mono<Boolean> insertMovil(String matricula);

}
