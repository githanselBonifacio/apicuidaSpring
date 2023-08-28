package co.com.sura.postgres.repository.remision.data;

import co.com.sura.entity.remision.Remision;
import com.google.gson.JsonObject;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;


public interface RemisionRepository extends ReactiveCrudRepository<RemisionData,String> {

    @Query("SELECT remision.*, " +
            "CONCAT(paciente.nombre, ' ', paciente.apellido) as paciente, "+
            "ciudad.nombre as ciudad "+
            "FROM public.remision " +
            "INNER JOIN ciudad ON remision.id_ciudad = ciudad.id_ciudad " +
            "INNER JOIN paciente ON remision.numero_identificacion_paciente = paciente.numero_identificacion " +
            "ORDER BY remision.fecha_admision; " )
    Flux<Remision> findAllRemision();

    @Query("CALL public.delete_remision_data(:idRemision,:numeroIdentificacionPaciente)")
    Mono<Void> deleteAllDataRemision(
            @Param("idRemision") String idRemision,
            @Param("numeroIdentificacionPaciente") String numeroIdentificacionPaciente);


    @Query("INSERT INTO remision(" +
            "id_remision, " +
            "estado, " +
            "fecha_admision, " +
            "programa, " +
            "tipo_admision, " +
            "institucion_remite, " +
            "numero_identificacion_paciente," +
            "id_ciudad) " +
            " VALUES " +
            "($1, $2, $3, $4, $5, $6, $7,$8)")
    Mono<Void> insertRemisionQuery(
            @Param("$1") String idRemision,
            @Param("$2") String estado,
            @Param("$3") LocalDate fechaAdmision,
            @Param("$4") String programa,
            @Param("$5") String tipoAdmision,
            @Param("$6") String institucionRemite,
            @Param("$7") String numeroIdentificacionPaciente,
            @Param("$8") String idCiudad
    );
    default Mono<Void> insertRemision(RemisionData remisionData){
        return insertRemisionQuery(
                remisionData.getIdRemision(),
                remisionData.getEstado(),
                remisionData.getFechaAdmision(),
                remisionData.getPrograma(),
                remisionData.getTipoAdmision(),
                remisionData.getInstitucionRemite(),
                remisionData.getNumeroIdentificacionPaciente(),
                remisionData.getIdCiudad()
        );
    }


}
