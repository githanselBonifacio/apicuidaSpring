package co.com.sura.postgres.repository.admin.data;

import co.com.sura.entity.admin.Remision;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;


public interface RemisionRepository extends ReactiveCrudRepository<RemisionData,String> {

    @Query("SELECT EXISTS(SELECT * FROM remisiones WHERE id_remision = $1 AND estado = 'ADMITIDO');")
    Mono<Boolean>validarEstadosRemisionToEgreso(String idRemision);

    @Query("UPDATE remisiones SET estado = 'EGRESADO' WHERE id_remision = $1;")
    Mono<Boolean> egresarRemisionById(String idRemision);

    @Query("SELECT remisiones.*, " +
            "CONCAT(pacientes.nombres, ' ', pacientes.apellidos) as paciente, "+
            "regionales.nombre as regional "+
            "FROM public.remisiones " +
            "INNER JOIN regionales ON remisiones.id_regional = regionales.id_regional " +
            "INNER JOIN pacientes ON remisiones.numero_identificacion_paciente = pacientes.numero_identificacion " +
            "ORDER BY remisiones.fecha_admision; " )
    Flux<Remision> findAllRemision();

    @Query("CALL public.delete_remision_data(:idRemision,:numeroIdentificacionPaciente)")
    Mono<Void> deleteAllDataRemision(
            @Param("idRemision") String idRemision,
            @Param("numeroIdentificacionPaciente") String numeroIdentificacionPaciente);


    @Query("INSERT INTO remisiones(id_remision) VALUES ($1)")
    Mono<Boolean> insertNuevaRemision(
            @Param("$1") String idRemision);

}
