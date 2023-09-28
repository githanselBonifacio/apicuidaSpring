package co.com.sura.postgres.repository.agenda.data;


import co.com.sura.entity.agenda.Profesional;
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
            "Where id_regional = $1;")
    Flux<ProfesionalData> findByIdRegional(@Param("$1") String idRegional);

    @Query("SELECT DISTINCT  public.profesionales.* " +
            "FROM public.profesionales " +
            "LEFT JOIN public.turno_profesional " +
            "on public.profesionales.numero_identificacion = public.turno_profesional.id_profesional " +
            "AND public.turno_profesional.fecha_turno = $1 "+
            "WHERE public.profesionales.id_regional = $2 " +
            "AND public.profesionales.activo = true " +
            "AND public.turno_profesional.id IS NULL;")
    Flux<ProfesionalData> findByTurnoRegional(
            @Param("$1") LocalDate fechaTurno,
            @Param("$2") String idRegional
    );
    @Query("SELECT * " +
            "FROM public.profesionales " +
            "INNER JOIN public.turno_profesional " +
            "on public.profesionales.numero_identificacion = public.turno_profesional.id_profesional " +
            "WHERE public.profesionales.id_regional = $2 " +
            "AND public.profesionales.activo = true " +
            "AND public.turno_profesional.id_horario_turno = $3" +
            "AND public.turno_profesional.fecha_turno = $1")
    Flux<ProfesionalData> findFromTurnoRegional(
            @Param("$1") LocalDate fechaTurno,
            @Param("$2") String idRegional,
            @Param("$3") Integer idHorarioTurno
    );

    @Query("INSERT INTO public.profesionales( " +
            " numero_identificacion, " +
            " id_tipo_identificacion, " +
            " nombres, apellidos, " +
            " fecha_nacimiento, " +
            " id_regional, activo, " +
            " genero, direccion, " +
            " email, celular, " +
            " telefono, id_profesion) " +
            " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13);")
    Mono<Void> insertQueryProfesional(
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
            @Param("$12") String telefono,
            @Param("$13") Integer idProfesional);

  default Mono<Void> insertProfesional (Profesional Profesional){
      return insertQueryProfesional(
                Profesional.getNumeroIdentificacion(),
                Profesional.getIdTipoIdentificacion(),
                Profesional.getNombres(),
                Profesional.getApellidos(),
                Profesional.getFechaNacimiento(),
                Profesional.getIdRegional(),
                Profesional.isActivo(),
                Profesional.getGenero(),
                Profesional.getDireccion(),
                Profesional.getEmail(),
                Profesional.getCelular(),
                Profesional.getTelefono(),
                Profesional.getIdProfesion()
        );
    }

}
