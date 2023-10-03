package co.com.sura.postgres.repository.maestros.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
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
}
