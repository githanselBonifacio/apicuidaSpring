package co.com.sura.postgres.repository.personal.data;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;


@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "items_secuencias_turno")
public class ItemSecuenciaTurnoData {

    @Column("nombre_secuencia")
    private String nombreSecuencia;
    private String descripcion;

    @Column("numero_dia")
    private Integer numeroDia;
    @Column("nombre_dia")
    private String nombreDia;

    @Column("id_horario_turno")
    private Integer idHorarioTurno;
}
