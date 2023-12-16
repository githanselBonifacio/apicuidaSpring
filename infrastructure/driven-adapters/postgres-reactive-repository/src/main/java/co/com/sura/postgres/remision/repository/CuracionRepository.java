package co.com.sura.postgres.remision.repository;

import co.com.sura.postgres.remision.data.CuracionData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface CuracionRepository extends ReactiveCrudRepository<CuracionData,Integer> {
    Flux<CuracionData> findByIdCita(String idCita);

    @Query("DELETE From curaciones WHERE id_cita LIKE concat($1 ,'-%')")
    Mono<Void> deleteByIDRemision(String idRemision);
}
