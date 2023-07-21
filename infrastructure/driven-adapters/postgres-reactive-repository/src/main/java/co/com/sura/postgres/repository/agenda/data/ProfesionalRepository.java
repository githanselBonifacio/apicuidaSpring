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

    @Query("SELECT DISTINCT  public.profesionales.* " +
            "FROM public.profesionales " +
            "LEFT JOIN public.turno_profesional " +
            "on public.profesionales.numero_identificacion = public.turno_profesional.id_profesional " +
            "AND public.turno_profesional.fecha_turno = $1 "+
            "WHERE public.profesionales.id_ciudad = $2 " +
            "AND public.profesionales.activo = true " +
            "AND public.turno_profesional.id IS NULL;")
    Flux<ProfesionalData> findByTurnoCiudad(
            @Param("$1") LocalDate fechaTurno,
            @Param("$2") String idCiudad
    );
    @Query("SELECT * " +
            "FROM public.profesionales " +
            "INNER JOIN public.turno_profesional " +
            "on public.profesionales.numero_identificacion = public.turno_profesional.id_profesional " +
            "WHERE public.profesionales.id_ciudad = $2 " +
            "AND public.profesionales.activo = true " +
            "AND public.turno_profesional.id_horario_turno = $3" +
            "AND public.turno_profesional.fecha_turno = $1")
    Flux<ProfesionalData> findFromTurnoCiudad(
            @Param("$1") LocalDate fechaTurno,
            @Param("$2") String idCiudad,
            @Param("$3") Integer idHorarioTurno
    );

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
