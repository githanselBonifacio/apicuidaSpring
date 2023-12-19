package co.com.sura.postgres.remision.repository.datospaciente;

import co.com.sura.postgres.remision.data.datospaciente.UbicacionData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;


public interface UbicacionRepository extends ReactiveCrudRepository<UbicacionData,String> {


   @Query("SELECT * FROM ubicaciones WHERE id_ubicacion LIKE CONCAT($1,'_%')")
   Mono<UbicacionData> findByIdRemision(String numeroIdentificacionPaciente);



   @Query("INSERT INTO ubicaciones (id_ubicacion) VALUES ($1)")
   Mono<Boolean> insertNuevaUbicacion(
           @Param("$1") String idUbicacion);
}
