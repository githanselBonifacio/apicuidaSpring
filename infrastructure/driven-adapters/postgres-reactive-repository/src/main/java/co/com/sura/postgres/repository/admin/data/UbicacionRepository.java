package co.com.sura.postgres.repository.admin.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;


public interface UbicacionRepository extends ReactiveCrudRepository<UbicacionData,String> {

   @Override
   Mono<Boolean> existsById(String idUbicacion);

   @Query("DELETE FROM public.ubicaciones WHERE id_ubicacion=$1;")
   Mono<Void> deleteByIdUbicacion(String idUbicacion);

   @Query("INSERT INTO ubicaciones (" +
           " id_ubicacion)" +
           " VALUES " +
           "($1)"
   )
   Mono<Boolean> insertNuevaUbicacion(
           @Param("$1") String idUbicacion);
}
