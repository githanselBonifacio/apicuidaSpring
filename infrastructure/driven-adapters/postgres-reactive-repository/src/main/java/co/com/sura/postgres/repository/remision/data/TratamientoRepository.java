package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface TratamientoRepository extends ReactiveCrudRepository<TratamientoData,Integer> {

    Flux<TratamientoData> findByIdCita(String idCita);
    @Query("UPDATE tratamiento " +
            "SET notificado = true " +
            "WHERE id_tratamiento=$1")
    Mono<Void> updateNotificar(Integer idTratamiento);
}
