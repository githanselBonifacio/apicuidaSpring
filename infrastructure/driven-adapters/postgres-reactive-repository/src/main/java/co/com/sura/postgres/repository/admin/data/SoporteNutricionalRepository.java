package co.com.sura.postgres.repository.admin.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface SoporteNutricionalRepository extends ReactiveCrudRepository<SoporteNutricionalData,Integer> {
    Flux<SoporteNutricionalData> findByIdCita(String idCita);
    @Query("UPDATE soporte_nutricionales " +
            "SET notificado = true " +
            "WHERE id_soporte_nutricional=$1")
    Mono<Void> updateNotificar(Integer idSoporteNutricional);
}
