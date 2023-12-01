package co.com.sura.postgres.repository.remision.repository;

import co.com.sura.postgres.repository.remision.data.CanalizacionData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface CanalizacionRepository extends ReactiveCrudRepository<CanalizacionData,Integer> {

    Flux<CanalizacionData> findByIdCita(String idCita);

    @Query("DELETE From canalizaciones WHERE id_cita LIKE concat($1 ,'-%')")
    Mono<Void> deleteByIDRemision(String idRemision);
}
