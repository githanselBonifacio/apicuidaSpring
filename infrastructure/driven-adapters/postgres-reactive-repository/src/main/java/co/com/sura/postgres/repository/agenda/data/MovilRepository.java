package co.com.sura.postgres.repository.agenda.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface MovilRepository extends ReactiveCrudRepository<MovilData,String> {
    Flux<MovilData> findByIdRegional(String idRegional);

   @Query("SELECT public.moviles.*  from moviles " +
           "LEFT JOIN public.conductores ON public.conductores.matricula_movil = public.moviles.matricula " +
           "WHERE public.conductores.numero_identificacion is null;")
   Flux<MovilData> findAllWithoutConductor();
}
