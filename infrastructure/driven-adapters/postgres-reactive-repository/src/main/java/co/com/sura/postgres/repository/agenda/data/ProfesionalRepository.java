package co.com.sura.postgres.repository.agenda.data;


import co.com.sura.entity.agenda.Profesional;
import co.com.sura.postgres.repository.maestros.data.CiudadData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;


public interface ProfesionalRepository extends ReactiveCrudRepository<ProfesionalData,String> {

    Flux<ProfesionalData> findAll();

    @Query("SELECT * " +
            "FROM public.profesionales " +
            "Where id_ciudad = $1;")
    Flux<ProfesionalData> findByIdCiudad(  @Param("$1") String idCiudad);

    Mono<ProfesionalData> save(ProfesionalData profesionalData);

    @Query("INSERT INTO profesionales " +
            "(numero_identificacion, id_tipo_identificacion, nombre, apellido, fecha_nacimiento, id_ciudad)" +
            " VALUES ($1, $2,$3, $4,$5, $6)")
    Mono<Profesional> insertProfesional(
            @Param("$1") String numeroIdentificacion,
            @Param("$2") Integer idTipoIdentificacion,
            @Param("$3") String nombre,
            @Param("$4") String apellido,
            @Param("$5") LocalDate fechaNacimiento,
            @Param("$6") String idCiudad);

}
