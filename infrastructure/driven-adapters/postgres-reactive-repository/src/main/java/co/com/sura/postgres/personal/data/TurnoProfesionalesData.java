package co.com.sura.postgres.personal.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;
import java.time.LocalDate;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "turno_profesional")
public class TurnoProfesionalesData {
    @Id
    @Column("id")
    private Integer idTurno;

    @Column("fecha_turno")
    private LocalDate fechaTurno;

    @Column("id_horario_turno")
    private Integer idHorarioTurno;

    @Column("id_profesional")
    private String idProfesional;

    @Column("id_regional")
    private  String idRegional;

}
