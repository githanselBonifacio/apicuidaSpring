package co.com.sura.postgres.repository.personal.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;


public interface ConductorRepository extends ReactiveCrudRepository<ConductorData,String> {
    @Query("INSERT INTO public.conductores( numero_identificacion) VALUES ($1);")
    Mono<Void> insertConductor(@Param("$1") String numeroIdentificacion);

}
