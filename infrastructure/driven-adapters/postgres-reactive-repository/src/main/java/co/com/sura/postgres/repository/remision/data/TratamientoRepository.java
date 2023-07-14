package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface TratamientoRepository extends ReactiveCrudRepository<TratamientoData,Integer> {

    Flux<TratamientoData> findByIdCita(String idCita);
}
