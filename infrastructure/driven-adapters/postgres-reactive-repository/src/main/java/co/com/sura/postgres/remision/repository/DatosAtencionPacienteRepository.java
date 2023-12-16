package co.com.sura.postgres.remision.repository;

import co.com.sura.postgres.remision.data.DatosAtencionPacienteData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;


public interface DatosAtencionPacienteRepository extends ReactiveCrudRepository <DatosAtencionPacienteData,Integer> {

    Mono<DatosAtencionPacienteData> findByIdRemision(String idRemision);

    @Query("UPDATE public.datos_atencion_paciente " +
            "SET nombre_cuidador=$1, nombre_responsable=$2, telefono_paciente=$3, celular_paciente=$4," +
            " celular_paciente2=$5, id_remision=$6 " +
            "WHERE id_remision=$6;")
    Mono<Void>updateDatosAtencionQuery(
            @Param("$1") String nombreCuidador,
            @Param("$2") String nombreResponsable,
            @Param("$3") String telefonoPaciente,
            @Param("$4") String celularPaciente,
            @Param("$5") String celularPaciente2,
            @Param("$6") String idRemision);

    default Mono<Void> updateDatosAtencion(DatosAtencionPacienteData datosAtencionPacienteData){
        return  updateDatosAtencionQuery(
                datosAtencionPacienteData.getNombreCuidador(),
                datosAtencionPacienteData.getNombreResponsable(),
                datosAtencionPacienteData.getTelefonoPaciente(),
                datosAtencionPacienteData.getCelularPaciente(),
                datosAtencionPacienteData.getCelularPaciente2(),
                datosAtencionPacienteData.getIdRemision()
        );
    }
    @Query("DELETE FROM datos_atencion_paciente WHERE id_remision = $1")
    Mono<Void> deleteByIdRemision(String idRemision);
}
