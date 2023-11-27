package co.com.sura.postgres.repository.maestros.data;

import co.com.sura.postgres.repository.agenda.data.CitaData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import java.time.LocalDateTime;
import java.time.LocalTime;


@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "horarios_turno")
public class HorarioTurnoData {

    @Id
    private Integer id;
    private String nombre;

    @Temporal(TemporalType.TIME)
    private LocalTime horaInicio;

    @Temporal(TemporalType.TIME)
    private LocalTime horaFin;

    @Column("color_hex_referencia")
    private String colorHexReferencia;

    @Column("es_horario_base")
    private Boolean esHorarioBase;

    private String descripcion;

    @Column("duracion_horas")
    private Integer duracionHoras;

    public static Boolean validarHorarioCita(LocalDateTime fechaCita, HorarioTurnoData horario, Integer duracionCita){
        return fechaCita.toLocalTime().isAfter(horario.getHoraInicio().minusMinutes(1)) &&
                fechaCita.toLocalTime().plusSeconds(duracionCita).isBefore(horario.getHoraFin());
    }
}
