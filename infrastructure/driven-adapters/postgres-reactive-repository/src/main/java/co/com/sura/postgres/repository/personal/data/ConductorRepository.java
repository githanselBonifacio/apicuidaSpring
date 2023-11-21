package co.com.sura.postgres.repository.personal.data;

import co.com.sura.entity.personal.Conductor;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface ConductorRepository extends ReactiveCrudRepository<ConductorData,String> {
    @Query("INSERT INTO public.conductores( " +
            " numero_identificacion, " +
            " id_tipo_identificacion, " +
            " nombres, apellidos, " +
            " fecha_nacimiento, " +
            " id_regional, activo, " +
            " genero, direccion, " +
            " email, celular, " +
            " telefono) " +
            " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12);")
    Mono<Void> insertQueryConductor(
            @Param("$1") String numeroIdentificacion,
            @Param("$2") Integer idTipoIdentificacion,
            @Param("$3") String nombres,
            @Param("$4") String apellidos,
            @Param("$5") LocalDate fechaNacimiento,
            @Param("$6") String idRegional,
            @Param("$7") boolean activo,
            @Param("$8") String genero,
            @Param("$9") String direccion,
            @Param("$10") String email,
            @Param("$11") String celular,
            @Param("$12") String telefono);

    default Mono<Void> insertConductor (Conductor conductor){
        return insertQueryConductor(
                conductor.getNumeroIdentificacion(),
                conductor.getIdTipoIdentificacion(),
                conductor.getNombres(),
                conductor.getApellidos(),
                conductor.getFechaNacimiento(),
                conductor.getIdRegional(),
                conductor.isActivo(),
                conductor.getGenero(),
                conductor.getDireccion(),
                conductor.getEmail(),
                conductor.getCelular(),
                conductor.getTelefono()
        );
    }
}
