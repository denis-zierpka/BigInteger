#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#define PP( expr ) std::cerr<<#expr<<" "<<expr<<" ";


class BigInteger {
    friend std::ostream& operator << (std::ostream& cout, const BigInteger& output);

public:
    static const int NUMBER_COUNT;
    static const int BASE;

private:
    std::vector<int> body_;
    bool negative_;

public:
    BigInteger(): body_({0}), negative_(false) {}

    BigInteger(const BigInteger& other): body_(other.body_), negative_(other.negative_) {}

    BigInteger(const std::string& value): BigInteger() {
        body_.clear();
        negative_ = false;
        int is_negative = 0;
        if (value[0] == '-') {
            negative_ = true;
            is_negative = 1;
        }
        for (int i = static_cast<int>(value.size()) - 1; i >= is_negative; --i) {
            int max_number_of_symbols = std::min(NUMBER_COUNT, i - is_negative + 1);
            int new_number = 0;
            int base_change = 1;
            for (int j = i; j > i - max_number_of_symbols; --j) {
                new_number += (value[j] - '0') * base_change;
                base_change *= 10;
            }
            i -= max_number_of_symbols - 1;
            body_.push_back(new_number);
        }
        normalize();
    }

    BigInteger(long long element): BigInteger(std::to_string(element)) {}

    BigInteger& operator= (const BigInteger& other) {
        if (this == &other) return *this;
        body_ = other.body_;
        negative_ = other.negative_;
        normalize();
        return *this;
    }

    std::string toString() const {
        std::string new_object;
        if (negative())
            new_object += '-';
        int max_size = static_cast<int>(body_.size()) - 1;
        for (int i = max_size; i >= 0; --i) {
            if (i != max_size) {
                std::string base_number = std::to_string(body_[i]);
                for (int j = 0; j < BigInteger::NUMBER_COUNT - static_cast<int>(base_number.size()); ++j)
                    new_object += '0';
            }
            new_object += std::to_string(body_[i]);
        }
        return new_object;
    }

    bool negative() const {
        return negative_;
    }

    BigInteger abs() const {
        BigInteger new_object = *this;
        if (new_object.negative()) {
            new_object.change_sign();
            new_object.normalize();
        }
        return new_object;
    }

    void change_sign() {
        negative_ = !negative_;
    }

    int size() const {
        return static_cast<int>(body_.size());
    }

    BigInteger operator-() const {
        //std::cerr << "- " << *this << ' ';
        //PP(this);
        //std::cerr << std::endl;
        BigInteger new_object = *this;
        new_object.negative_ = !new_object.negative_;
        new_object.normalize();
        //std::cerr << *this << std::endl;
        return new_object;
    }

    void normalize() {
        while (size() > 1 && body_[size() - 1] == 0)
            body_.pop_back();
        if (size() == 0) {
            body_.push_back(0);
            negative_ = false;
        } else if (size() == 1 && body_[0] == 0) {
            negative_ = false;
        }
    }

    bool more_if_equal_sign(const BigInteger& other) const {
        if (size() > other.size()) {
            return true;
        } else if (size() < other.size()) {
            return false;
        } else {
            for (int i = size() - 1; i >= 0; --i) {
                if (body_[i] < other.body_[i])
                    return false;
                if (body_[i] > other.body_[i])
                    return true;
            }
            return false;
        }
    }

    bool operator> (const BigInteger& other) const {
        if (!negative() && other.negative()) {
            return true;
        } else if (negative() && !other.negative()) {
            return false;
        } else if (!negative() && !other.negative()) {
            return more_if_equal_sign(other);
        } else {
            return other.more_if_equal_sign(*this);
        }
    }

    explicit operator bool() const {
        return (*this != 0);
    }

    bool operator< (const BigInteger& other) const {
        return (other > *this);
    }

    bool operator== (const BigInteger& other) const {
        return (size() == other.size() && !(*this > other) && !(other > *this));
    }

    bool operator>= (const BigInteger& other) const {
        return (*this > other || *this == other);
    }

    bool operator<= (const BigInteger& other) const {
        return (*this < other || *this == other);
    }

    bool operator!= (const BigInteger& other) const {
        return !(*this == other);
    }

    void plus_if_same_sign(const BigInteger& other) {
        while (other.size() > size())
            body_.push_back(0);
        for (int i = 0; i < size(); ++i) {
            if (i < other.size())
                body_[i] += other.body_[i];
        }
        for (int i = 0; i < size(); ++i) {
            if (body_[i] >= BASE) {
                if (i + 1 < size()) {
                    body_[i + 1] += body_[i] / BASE;
                    body_[i] %= BASE;
                } else {
                    body_.push_back(body_[i] / BASE);
                    body_[i] %= BASE;
                }
            }
        }
        normalize();
    }

    void minus_if_same_sign(const BigInteger& other) {
        bool second_more = false;
        if (!more_if_equal_sign(other)) {
            second_more = true;
        }
        while (other.size() > size())
            body_.push_back(0);
        for (int i = 0; i < std::max(size(), other.size()); ++i) {
            if (i < other.size()) {
                if (second_more) {
                    body_[i] += other.body_[i] - 2 * body_[i];
                } else {
                    body_[i] -= other.body_[i];
                }
            }
        }
        for (int i = 0; i < size(); ++i) {
            if (body_[i] < 0) {
                body_[i + 1] -= std::abs(body_[i]) / BASE;
                body_[i] %= BASE;
                if (body_[i] < 0) {
                    body_[i] += BASE;
                    body_[i + 1]--;
                }
            }
        }
        if (second_more)
            negative_ = !negative_;
        normalize();
    }

    BigInteger& operator+= (const BigInteger& other) {
        //std::cerr << "+= " << *this << ' ' << other << ' ';
        //PP(this);
        //PP(&other);
        //std::cerr << std::endl;
        if ((negative() && other.negative()) || (!negative() && !other.negative())) {
            plus_if_same_sign(other);
        } else {
            minus_if_same_sign(other);
        }
        //std::cerr << *this << std::endl;
        return *this;
    }

    BigInteger& operator++() {
        *this += 1;
        normalize();
        return *this;
    }

    BigInteger operator++(int) {
        BigInteger new_object = *this;
        *this += 1;
        normalize();
        return new_object;
    }

    BigInteger& operator--() {
        *this -= 1;
        normalize();
        return *this;
    }

    BigInteger operator--(int) {
        BigInteger new_object = *this;
        *this -= 1;
        normalize();
        return new_object;
    }

    BigInteger& operator-= (const BigInteger& other) {
        //std::cerr << "-= " << *this << ' ' << other << ' ';
        //PP(this);
        //PP(&other);
        //std::cerr << std::endl;
        if ((!negative() && !other.negative()) || (negative() && other.negative())) {
            minus_if_same_sign(other);
        } else {
            plus_if_same_sign(other);
        }
        //std::cerr << *this << std::endl;
        return *this;
    }

    BigInteger& operator*= (const BigInteger& other) {
        //std::cerr << "*= " << *this << ' ' << other << ' ';
        //PP(this);
        //PP(&other);
        //std::cerr << std::endl;
        int new_size = size() + other.size() + 2;
        BigInteger new_body = BigInteger();
        new_body.body_.resize(new_size);
        for (int i = 0; i < size(); ++i) {
            for (int j = 0; j < other.size(); ++j) {
                new_body.body_[i + j] += body_[i] * other.body_[j];
            }
        }
        for (int i = 0; i < new_body.size() - 1; ++i) {
            if (new_body.body_[i] >= BASE) {
                new_body.body_[i + 1] += new_body.body_[i] / BASE;
                new_body.body_[i] %= BASE;
            }
        }
        new_body.negative_ = (negative_ != other.negative_);
        *this = new_body;
        normalize();
        //std::cerr << *this << std::endl;
        return *this;
    }

    void division(const BigInteger& other, BigInteger& a, BigInteger& new_body) {
        if (*this == other) {
            a = 0;
            new_body = 1;
            return;
        }
        if (other == 0) {
            a = *this;
            new_body = *this;
            return;
        }
        if (other == 1 || other == -1) {
            a = 0;
            new_body = *this;
            new_body.negative_ = (negative_ != other.negative_);
            return;
        }
        a = 0;
        new_body = 0;
        for (int i = size() - 1; i >= 0; --i) {
            a *= BASE;
            a += body_[i];
            int q = 0;
            while (a.more_if_equal_sign(other) || a == other || a == -other) {
                if (!other.negative())
                    a -= other;
                else
                    a += other;
                ++q;
            }
            new_body *= BASE;
            new_body += q;
        }
        new_body.negative_ = (negative_ != other.negative_);
        a.negative_ = negative_;
    }

    BigInteger& operator/= (const BigInteger& other) {
        //std::cerr << "/= " << *this << ' ' << other << ' ';
        //PP(this);
        //PP(&other);
        //std::cerr << std::endl;
        BigInteger a = BigInteger();
        BigInteger new_body = BigInteger();
        division(other, a, new_body);
        *this = new_body;
        normalize();
        //std::cerr << *this << std::endl;
        return *this;
    }

    BigInteger& operator%= (const BigInteger& other) {
        //std::cerr << "%= " << *this << ' ' << other << ' ';
        //PP(this);
        //PP(&other);
        //std::cerr<< std::endl;
        BigInteger a = BigInteger();
        BigInteger new_body = BigInteger();
        division(other, a, new_body);
        *this = a;
        normalize();
        //std::cerr << *this << std::endl;
        return *this;
    }
};

const int BigInteger::NUMBER_COUNT = 1;
const int BigInteger::BASE = 10;

BigInteger operator"" _bi(unsigned long long element) {
    BigInteger new_object = element;
    return new_object;
}

BigInteger operator+ (const BigInteger& object, const BigInteger& other) {
    BigInteger new_object = object;
    new_object += other;
    return new_object;
}

BigInteger operator- (const BigInteger& object, const BigInteger& other) {
    BigInteger new_object = object;
    new_object -= other;
    return new_object;
}

BigInteger operator* (const BigInteger& object, const BigInteger& other) {
    BigInteger new_object = object;
    new_object *= other;
    return new_object;
}

BigInteger operator/ (const BigInteger& object, const BigInteger& other) {
    BigInteger new_object = object;
    new_object /= other;
    return new_object;
}

BigInteger operator% (const BigInteger& object, const BigInteger& other) {
    BigInteger new_object = object;
    new_object %= other;
    return new_object;
}

std::istream& operator >> (std::istream& in, BigInteger& input) {
    std::string new_input;
    in >> new_input;
    input = BigInteger(new_input);
    input.normalize();
    return in;
}

std::ostream& operator << (std::ostream& out, const BigInteger& output) {
    std::string result = output.toString();
    out << result;
    return out;
}




class Rational {
public:
    static const int NUMBER_COUNT;
    static const int BASE;

public:   // TODO
    BigInteger numerator_;
    BigInteger denominator_;

public:
    Rational(): numerator_(0), denominator_(1) {
        //std::cerr << "Rat0" << std::endl;
    }

    Rational(const Rational& other): numerator_(other.numerator_),
            denominator_(other.denominator_) {
        //std::cerr << "Rat1" << std::endl;
        normalize();
    }

    Rational(const BigInteger& element):
            numerator_(element), denominator_(1) {
        //std::cerr << "Rat2" << std::endl;
        normalize();
    }

    Rational(const int element):
            numerator_(element), denominator_(1) {
        //std::cerr << "Rat3" << std::endl;
        normalize();
    }

    Rational& operator= (const Rational& other) {
        if (this == &other) return *this;
        numerator_ = other.numerator_;
        denominator_ = other.denominator_;
        normalize();
        return *this;
    }

    std::string toString() const {
        std::string new_object;
        new_object += numerator_.toString();
        if (denominator_ != 1) {
            new_object += '/';
            new_object += denominator_.toString();
        }
        return new_object;
    }

    std::string asDecimal(size_t precision = 0) const {
        BigInteger max_real = numerator_.abs() / denominator_;
        BigInteger rest = numerator_.abs() % denominator_;
        std::string new_object;
        if (numerator_.negative())
            new_object += '-';
        new_object += max_real.toString();
        if (precision > 0) {
            new_object += '.';
            BigInteger base_number = 10;
            for (size_t i = 0; i < precision; ++i) {
                rest *= base_number;
                new_object += (rest / denominator_).toString();
                rest %= denominator_;
            }
        }
        return new_object;
    }

    explicit operator double () const {
        std::stringstream stream;
        stream << asDecimal(100);
        double result;
        stream >> result;
        return result;
    }

    BigInteger gcd(const BigInteger& a, const BigInteger& b) {
        return (a != 0 ? gcd(b % a, a) : b);
    }

    void normalize() {
        if (denominator_.negative()) {
            denominator_.change_sign();
            numerator_.change_sign();
        }
        numerator_.normalize();
        denominator_.normalize();
        BigInteger gcd_save = gcd(numerator_.abs(), denominator_.abs());
        if (gcd_save > 1) {
            numerator_ /= gcd_save;
            denominator_ /= gcd_save;
        }
    }

    Rational operator-() const {
        Rational new_object = *this;
        new_object.numerator_ = -new_object.numerator_;
        new_object.normalize();
        return new_object;
    }

    explicit operator bool() const {
        return (*this != 0);
    }

    bool operator> (const Rational& other) const {
        return ((numerator_ * other.denominator_) > (other.numerator_ * denominator_));
    }

    bool operator< (const Rational& other) const {
        return ((numerator_ * other.denominator_) < (other.numerator_ * denominator_));
    }

    bool operator== (const Rational& other) const {
        return (numerator_ == other.numerator_ && denominator_ == other.denominator_);
    }

    bool operator>= (const Rational& other) const {
        return (*this > other || *this == other);
    }

    bool operator<= (const Rational& other) const {
        return (*this < other || *this == other);
    }

    bool operator!= (const Rational& other) const {
        return !(*this == other);
    }

    Rational& operator+= (const Rational& other) {
        numerator_ *= other.denominator_;
        numerator_ += other.numerator_ * denominator_;
        denominator_ *= other.denominator_;
        normalize();
        return *this;
    }

    Rational& operator-= (const Rational& other) {
        numerator_ *= other.denominator_;
        numerator_ -= other.numerator_ * denominator_;
        denominator_ *= other.denominator_;
        normalize();
        return *this;
    }

    Rational& operator*= (const Rational& other) {
        numerator_ *= other.numerator_;
        denominator_ *= other.denominator_;
        normalize();
        return *this;
    }

    Rational& operator/= (const Rational& other) {
        numerator_ *= other.denominator_;
        denominator_ *= other.numerator_;
        normalize();
        return *this;
    }
};

Rational operator+ (const Rational& object, const Rational& other) {
    Rational new_object = object;
    new_object += other;
    return new_object;
}

Rational operator- (const Rational& object, const Rational& other) {
    Rational new_object = object;
    new_object -= other;
    return new_object;
}

Rational operator* (const Rational& object, const Rational& other) {
    Rational new_object = object;
    new_object *= other;
    return new_object;
}

Rational operator/ (const Rational& object, const Rational& other) {
    Rational new_object = object;
    new_object /= other;
    return new_object;
}

std::istream& operator >> (std::istream& in, Rational& input) {
    std::cerr << "operator cin for Rational" << std::endl;
    std::string new_input;
    in >> new_input;
    input = BigInteger(new_input);
    input.normalize();
    return in;
}

std::ostream& operator << (std::ostream& out, const Rational& output) {
    out << output.toString();
    return out;
}