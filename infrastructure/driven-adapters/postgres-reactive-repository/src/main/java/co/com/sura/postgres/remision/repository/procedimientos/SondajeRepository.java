package co.com.sura.postgres.remision.repository.procedimientos;

import co.com.sura.postgres.remision.data.procedimientos.SondajeData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface SondajeRepository extends ReactiveCrudRepository<SondajeData,Integer> {
    Flux<SondajeData> findByIdCita(String idCita);

    @Query("DELETE From sondajes WHERE id_cita LIKE concat($1 ,'-%')")
    Mono<Void> deleteByIDRemision(String idRemision);

    @Query("DELETE From sondajes WHERE id_cita = $1")
    Mono<Void> deleteByIdCita(String idCita);
}
