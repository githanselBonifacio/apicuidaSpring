package co.com.sura.postgres.repository.remision.data;


import co.com.sura.entity.remision.Paciente;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface PacienteRepository extends ReactiveCrudRepository<PacienteData,String> {

    @Query("SELECT public.paciente.*, to_json(public.ubicacion.*) as id_ubicacion " +
            "FROM public.remision " +
            "INNER JOIN public.paciente ON public.paciente.numero_identificacion = " +
            "public.remision.numero_identificacion_paciente " +
            "INNER JOIN public.ubicacion ON public.paciente.id_ubicacion = public.ubicacion.id_ubicacion " +
            "WHERE public.remision.id_remision = $1;")
    Mono<PacienteData> findPacienteByNumeroIdRemision(String idRemision);

    @Override
    Mono<Boolean> existsById(String numeroIdentificacion);

    @Query("INSERT INTO paciente( " +
            " numero_identificacion," +
            " tipo_identificacion," +
            " nombre," +
            " apellido," +
            " edad," +
            " sexo," +
            " peso," +
            " tipo_afiliacion," +
            " nombre_aseguradora," +
            " fecha_nacimiento," +
            " id_ubicacion) " +
            " VALUES " +
            "($1, $2, $3, $4, $5, $6, $7, $8, $9, $10,$11)"
    )
    Mono<Void> insertPacienteQuery(
            @Param("$1")   String numeroIdentificacion,
            @Param("$2")   String tipoIdentificacion,
            @Param("$3")   String nombre,
            @Param("$4")   String apellido,
            @Param("$5")   String edad,
            @Param("$6")   String sexo,
            @Param("$7")   String peso,
            @Param("$8")   String tipoAfiliacion,
            @Param("$9")   String nombreAseguradora,
            @Param("$10")  LocalDate fechaNacimiento,
            @Param("$11")  String idUbicacion
    );
    default Mono<Void> insertpaciente(PacienteData pacienteData){
        return insertPacienteQuery(
                pacienteData.getNumeroIdentificacion(),
                pacienteData.getTipoIdentificacion(),
                pacienteData.getNombre(),
                pacienteData.getApellido(),
                pacienteData.getEdad(),
                pacienteData.getSexo(),
                pacienteData.getPeso(),
                pacienteData.getTipoAfiliacion(),
                pacienteData.getNombreAseguradora(),
                pacienteData.getFechaNacimiento(),
                pacienteData.getIdUbicacion()
        );
    }
}
