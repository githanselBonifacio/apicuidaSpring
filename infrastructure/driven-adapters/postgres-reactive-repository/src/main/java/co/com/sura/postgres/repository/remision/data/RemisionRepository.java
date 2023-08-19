package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface RemisionRepository extends ReactiveCrudRepository<RemisionData,String> {

    @Query("CALL public.delete_remision_data(:idRemision,:numeroIdentificacionPaciente)")
    Mono<Void> deleteAllDataRemision(
            @Param("idRemision") String idRemision,
            @Param("numeroIdentificacionPaciente") String numeroIdentificacionPaciente);

    @Override
    Mono<Boolean> existsById(String idRemision);

    @Query("INSERT INTO remision(" +
            "id_remision, " +
            "estado, " +
            "fecha_admision, " +
            "programa, " +
            "tipo_admision, " +
            "institucion_remite, " +
            "numero_identificacion_paciente) " +
            " VALUES " +
            "($1, $2, $3, $4, $5, $6, $7)")
    Mono<Void> insertRemisionQuery(
            @Param("$1") String idRemision,
            @Param("$2") String estado,
            @Param("$3") LocalDate fechaAdmision,
            @Param("$4") String programa,
            @Param("$5") String tipoAdmision,
            @Param("$6") String institucionRemite,
            @Param("$7") String numeroIdentificacionPaciente
    );
    default Mono<Void> insertRemision(RemisionData remisionData){
        return insertRemisionQuery(
                remisionData.getIdRemision(),
                remisionData.getEstado(),
                remisionData.getFechaAdmision(),
                remisionData.getPrograma(),
                remisionData.getTipoAdmision(),
                remisionData.getInstitucionRemite(),
                remisionData.getNumeroIdentificacionPaciente()
        );
    }

}
