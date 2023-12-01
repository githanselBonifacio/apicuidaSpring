package co.com.sura.postgres.repository.personal.repository;

import co.com.sura.postgres.repository.personal.data.ItemSecuenciaTurnoData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;

public interface ItemSecuenciaTurnoRepository extends ReactiveCrudRepository<ItemSecuenciaTurnoData,Integer> {

    @Query("DELETE FROM public.items_secuencias_turno WHERE nombre_secuencia = $1;")
    Mono<Void> deleteByNombreSecuencia(String nombreSecuencia);
}
