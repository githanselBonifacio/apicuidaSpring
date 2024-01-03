package co.com.sura.postgres.remision.repository.tratamientos;

import co.com.sura.postgres.remision.data.tratamientos.TratamientoData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface TratamientoRepository extends ReactiveCrudRepository<TratamientoData,Integer> {

    Flux<TratamientoData> findByIdCita(String idCita);
    @Query("UPDATE tratamientos " +
            "SET notificado = true " +
            "WHERE id_tratamiento=$1")
    Mono<Void> updateNotificar(Integer idTratamiento);

    @Query("DELETE From tratamientos WHERE id_cita LIKE concat($1 ,'-%')")
    Mono<Void> deleteByIdRemision(String idRemision);
    @Query("DELETE From tratamientos WHERE id_cita = $1")
    Mono<Void> deleteByIdCita(String idCita);

}
